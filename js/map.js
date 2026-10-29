const collectionJson = 'intermediates/final_dat.json';
const countriesCsv = 'data/countries.csv';

function safeText(value) {
  return String(value ?? '').replace(/[&<>"']/g, char => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
  })[char]);
}

function coordinates(lat, lng) {
  if(lat == null || lng == null ||
    String(lat).trim() === '' || String(lng).trim() === '') {
    return null;
  }
  const latitude = Number(lat);
  const longitude = Number(lng);
  return Number.isFinite(latitude) && Number.isFinite(longitude)
    ? [latitude, longitude]
    : null;
}

async function fetchFile(url) {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`Could not load ${url}: HTTP ${response.status}`);
  return response;
}

function labelMarker(point, html) {
  return L.marker(point, {
    icon: L.divIcon({ className: 'empty-icon', html: '', iconSize: [0, 0] })
  }).bindTooltip(html, {
    permanent: true,
    direction: 'center',
    className: 'custom-map-label',
    interactive: true,
    opacity: 1
  });
}

async function loadMap() {
  const status = document.getElementById('map-status');

  try {
    const [collectionResponse, countryResponse] = await Promise.all([
      fetchFile(collectionJson),
      fetchFile(countriesCsv)
    ]);
    const allCollections = Object.values(await collectionResponse.json());
    const countries = await countryResponse.text();
    const parsed = Papa.parse(countries, {
      header: true,
      skipEmptyLines: true,
      transformHeader: header => header.trim().replace(/^\uFEFF/, '')
    });
    if (parsed.errors.length > 0) {
      throw new Error(`Invalid country CSV: ${parsed.errors[0].message}`);
    }

    const countryCentres = new Map(
      parsed.data.map(row => [String(row.Country ?? '').trim(), coordinates(row.clat, row.clong)])
    );

    const map = L.map('main-map', { preferCanvas: true, minZoom: 3, maxZoom: 12 })
      .setView([55, 15], 4);
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);

    const countryLayer = L.layerGroup();
    const cityLayer = L.layerGroup();
    const collectionLayer = L.layerGroup();

    // Collection markers
    allCollections.forEach(row => {
      const point = coordinates(row.lat, row.long);
      if (!point) return;
      const id = String(row.ID ?? '');
      const html = `<div class="marker-container"><a class="marker-link" target="_blank" rel="noopener" href="collection.html?${encodeURIComponent(id)}">${safeText(id)}</a><span class="city-tooltip"></span></div>`;
      labelMarker(point, html).addTo(collectionLayer);
    });

    // City markers
    const cities = new Map();
    allCollections.forEach(row => {
      const point = coordinates(row.clat, row.clong);
      if (!point) return;
      const key = JSON.stringify([row.City, row.Country, row.clong, row.clat, row.LID]);
      if (!cities.has(key)) cities.set(key, { point, city: row.City, lid: row.LID, count: 0 });
      cities.get(key).count += 1;
    });
    cities.forEach(({ point, city, lid, count }) => {
      const html = `<div class="marker-container"><a class="marker-link" target="_blank" rel="noopener" href="city.html?${encodeURIComponent(String(lid ?? ''))}">${safeText(city)}: ${count}</a><span class="city-tooltip"></span></div>`;
      labelMarker(point, html).addTo(cityLayer);
    });

    // Country markers
    const countryCounts = new Map();
    allCollections.forEach(row => {
      const country = String(row.Country ?? '').trim();
      countryCounts.set(country, (countryCounts.get(country) || 0) + 1);
    });
    countryCounts.forEach((count, country) => {
      const point = countryCentres.get(country);
      if (!point) return;
      const html = `<div class="marker-container country-zoom" data-lat="${point[0]}" data-lng="${point[1]}">${safeText(country)}: ${count}</div>`;
      labelMarker(point, html).addTo(countryLayer);
    });

    const layers = [countryLayer, cityLayer, collectionLayer];
    function updateVisibleLayer() {
      const zoom = map.getZoom();
      const visible = zoom <= 5 ? countryLayer : zoom <= 9 ? cityLayer : collectionLayer;
      layers.forEach(layer => {
        if (layer === visible && !map.hasLayer(layer)) map.addLayer(layer);
        if (layer !== visible && map.hasLayer(layer)) map.removeLayer(layer);
      });
    }
    map.on('zoomend', updateVisibleLayer);

    map.getContainer().addEventListener('click', event => {
      const el = event.target.closest('.country-zoom');
      if (!el) return;
      event.preventDefault();
      event.stopPropagation();
      map.setView([Number(el.dataset.lat), Number(el.dataset.lng)], 6);
    }, true);

    L.easyBar([
      L.easyButton('<span>Country</span>', (button, map) => map.setZoom(4)),
      L.easyButton('<span>City</span>', (button, map) => map.setZoom(6)),
      L.easyButton('<span>Collection</span>', (button, map) => map.setZoom(10))
    ], { position: 'topleft' }).addTo(map);

    const params = new URLSearchParams(window.location.search);
    const target = coordinates(params.get('lat'), params.get('lng'));
    if (target) map.setView(target, 11);

    updateVisibleLayer();
    status.hidden = true;
    requestAnimationFrame(() => map.invalidateSize());
  } catch (error) {
    console.error('Could not load the main map:', error);
    status.textContent = `Could not load map data. ${error.message}`;
  }
}

document.addEventListener('DOMContentLoaded', loadMap);