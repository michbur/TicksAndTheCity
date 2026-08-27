async function fetchWithCache(file) {
    const cacheKey = `cached_csv_${file}`;
    const cachedData = sessionStorage.getItem(cacheKey);

    if (cachedData) {
        return JSON.parse(cachedData);
    }

    const response = await fetch(file);
    if (!response.ok) throw new Error("Cannot fetch file " + file);
    
    const jsonData = await response.json(); 
    
    try {
        sessionStorage.setItem(cacheKey, JSON.stringify(jsonData)); 
    } catch (storageError) {
        console.warn("Could not cache data (file might be too large).", storageError);
    }
    
    return jsonData;
}


/**
 * Reads the URL, fetches the data, and returns ONLY the resulting string.
 * 
 * @param {string} file - The path to your JSON file
 * @param {string} targetProperty - The property to extract (e.g., 'name')
 * @returns {Promise<string>} - The extracted value or an error message
 */
async function getResultValue(file, targetProperty) {
    const targetId = decodeURIComponent(window.location.search.substring(1));

    if (!targetId) {
        return "No ID provided in URL";
    }

    try {
        const jsonData = await fetchWithCache(file);
        
        const matchedRow = jsonData[targetId];
        
        if (matchedRow && matchedRow[targetProperty] !== undefined) {
            return matchedRow[targetProperty];
        } else {
            return "ID or property not found";
        }
        
    } catch (error) {
        console.error('Error processing data:', error);
        return "Error loading data";
    }
}

/**
 * ORCHESTRATOR: Gets the value using getResultValue, then updates the DOM.
 */
async function updateElementWithData(file, elementId, targetProperty = "name") {
    const targetElement = document.getElementById(elementId);

    if (!targetElement) {
        console.warn(`Element with ID "${elementId}" not found.`);
        return;
    }

    const resultValue = await getResultValue(file, targetProperty);
    
    targetElement.textContent = resultValue;
}

/**
 * Extends update logic to handle anchor tags by fetching both a text property and a link property.
 */
async function updateElementWithDataAndLink(file, elementId, textProperty, linkProperty, hrefPrefix = "", separator = "") {
    const targetElement = document.getElementById(elementId);
    if (!targetElement) return;

    // Fetch the text value
    const textValue = await getResultValue(file, textProperty);
    
    // Dynamically build the link value
    let linkValue;
    if (Array.isArray(linkProperty)) {
        const values = await Promise.all(linkProperty.map(prop => getResultValue(file, prop)));
        
        if (values.includes("ID or property not found")) {
            linkValue = "ID or property not found";
        } else {
            // Use the user-provided separator instead of hardcoding
            linkValue = values.join(separator); 
        }
    } else {
        linkValue = await getResultValue(file, linkProperty);
    }
    
    const notFoundMsg = "ID or property not found";

    // Update the DOM
    if (textValue && textValue !== notFoundMsg) {
        targetElement.textContent = textValue;
        if (linkValue && linkValue !== notFoundMsg) {
            targetElement.href = hrefPrefix + linkValue;
        }
    }
}

/**
 * Helper function to safely set the display state and optional text content of a DOM element.
 * 
 * @param {string} id - The HTML element ID
 * @param {string} state - The CSS display state (e.g., 'none', 'block')
 * @param {string} [text=null] - Optional text content to set
 */
function setDisplay(id, state, text = null) {
    const el = document.getElementById(id);
    if (el) {
        el.style.display = state;
        if (text) {
            el.textContent = text;
        }
    }
}
