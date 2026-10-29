const pubJson = 'intermediates/publications.json';
const collectionJson = 'intermediates/final_dat.json';

const textFields = [
  ['pub-title', 'ArticleTitle'],
  ['pub-abstract', 'AbstractText'],
  ['pub-journal', 'Title']
];

const linkFields = [
  ['pub-doi-link', 'DOI', 'DOI', 'https://doi.org/'],
  ['pub-pmid-link', 'PMID', 'PMID', 'https://pubmed.ncbi.nlm.nih.gov/']
];

// Find coordinates of collections in a publication
function getMapPoints(allCollections, targetDoi) {
  return Object.values(allCollections).filter(
      row => row.DOI === targetDoi && row.lat && row.long
    );
}

// Display map
function displayCollectionMap(mapPoints) {
  const mapContainer = document.getElementById('pub-map');
  mapContainer.style.display = 'block';

  const map = L.map('pub-map');
  L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map);

  const bounds = L.latLngBounds();

  mapPoints.forEach(pt => {
    const customHtml = `<div class="marker-container"><a href="collection.html?${pt.ID}" target="_blank" class="marker-link">${pt.ID}</a><span class="city-tooltip"></span></div>`;
  
    const invisibleIcon = L.divIcon({ 
      className: 'empty-icon', 
      html: '', 
      iconSize: [0, 0] 
    });

    L.marker([parseFloat(pt.lat), parseFloat(pt.long)], {icon: invisibleIcon})
      .bindTooltip(customHtml, {
        permanent: true,
        direction: 'center',
        className: 'custom-map-label',
        interactive: true
      }).addTo(map);
    
    bounds.extend([parseFloat(pt.lat), parseFloat(pt.long)]);
  });

  setTimeout(() => {
    map.invalidateSize();
    map.fitBounds(bounds, {maxZoom: 11, padding: [30, 30]});
  }, 100);
}

async function populatePublication() {
  const updates = [
    ...textFields.map(
      ([id, property]) => updateElementWithData(pubJson, id, property)
    ),
    ...linkFields.map(
      ([id, textProperty, linkProperty, prefix, separator]) =>
        updateElementWithDataAndLink(
          pubJson, id, textProperty, linkProperty, prefix
        )
    )
  ];

  await Promise.all(updates);
}

async function loadPublication() {
  try {
    const targetDoi = decodeURIComponent(window.location.search.substring(1))
    if (!targetDoi) {
      setDisplay('loading', 'none');
      setDisplay('error-message', 'block', "No DOI specified in the URL.");
      return;
    }
    
    await populatePublication();

    const titleText = document.getElementById('pub-title').textContent;
    if (titleText && titleText !== 'ID or property not found') {
      document.title = titleText;
    }

    setDisplay('loading', 'none');
    setDisplay('publication-info', 'block');
    
    // Find and display associated collections on the map
    const allCollections = await fetchWithCache(collectionJson);
    const mapPoints = getMapPoints(allCollections, targetDoi);
    
    if (mapPoints.length > 0) {
      displayCollectionMap(mapPoints);
    }

  } catch (error) {
    console.error(error);
    setDisplay('loading', 'none');
    setDisplay('error-message', 'block', 'Error loading publication data.');
  }
}

document.addEventListener('DOMContentLoaded', loadPublication);

