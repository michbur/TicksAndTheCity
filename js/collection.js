const jsonFile = "./intermediates/final_dat.json";

const textFields = [
  ['pub-title', 'ArticleTitle'],
  ['pub-country', 'Country'],
  ['pub-species', 'Tick species'],
  ['pub-source', 'Source'],
  ['pub-year-start', 'Collection year start'],
  ['pub-year-end', 'Collection year end'],
  ['pub-exact-location', 'Exact location'],
  ['pub-year', 'Publication year'],
  ['pub-id', 'ID']
];

const linkFields = [
  ['pub-city', 'City', 'LID', 'city.html?'],
  ['pub-doi', 'DOI', 'DOI', 'publication.html?'],
  ['pub-site', 'Collection site', ['lat', 'long'], 'map.html?lat=', '&lng=']
];

const coordFields = [
  ['pub-lat', 'lat'],
  ['pub-long', 'long']
];

// Formating coordinates to four decimal places
const formatCoord = (val) => {
  if (!val || val === "ID or property not found" || isNaN(val)) {
    return "Not available"; 
  }
  return parseFloat(val).toFixed(4);
};


// Updating coordinates
async function updateCoord(elementId, property) {
  const element = document.getElementById(elementId);

  if (!element) {
    console.warn(`Element "${elementId}" not found.`);
    return;
  }

  const value = await getResultValue(jsonFile, property);
  element.textContent = formatCoord(value);
}


// Updating all collection fields
async function populateCollection() {
  const updates = [
    ...textFields.map(
      ([id, property]) => updateElementWithData(jsonFile, id, property)
    ),
    ...linkFields.map(
      ([id, textProperty, linkProperty, prefix, separator]) =>
        updateElementWithDataAndLink(
          jsonFile, id, textProperty, linkProperty, prefix, separator
        )
    ),
    ...coordFields.map(
      ([id, property]) => updateCoord(id, property)
    )
  ];

  await Promise.all(updates);
}

async function loadCollection() {
  const loading = document.getElementById('loading');

  const loadingTimer = setTimeout(() => {
    loading.style.display = 'block';
  }, 400);

  try {
    const targetId = decodeURIComponent(window.location.search.substring(1));

    if (!targetId) {
      setDisplay('loading', 'none');
      setDisplay('error-message', 'block', "No publication ID specified in the URL.");
      return;
    }

    // Updating page content
    await populateCollection();

    setDisplay('loading', 'none');
    setDisplay('publication-card', 'block');
    
  } catch (error) {
    console.error(error);
    setDisplay('loading', 'none');
    setDisplay('error-message', 'block', "Error populating data on the page.");
  } finally {
    clearTimeout(loadingTimer);
    loading.style.display = 'none';
  }
}

document.addEventListener("DOMContentLoaded", loadCollection);
