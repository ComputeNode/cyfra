// DOM elements
const modeReal = document.getElementById('mode-real');
const modeSynthetic = document.getElementById('mode-synthetic');
const realControls = document.getElementById('real-controls');
const syntheticControls = document.getElementById('synthetic-controls');
const tileSearch = document.getElementById('tile-search');
const regionFilter = document.getElementById('region-filter');
const categoryFilter = document.getElementById('category-filter');
const countryFilter = document.getElementById('country-filter');
const tileSelect = document.getElementById('tile-select');
const tileCount = document.getElementById('tile-count');
const tileDetails = document.getElementById('tile-details');
const dateInput = document.getElementById('date-input');
const widthInput = document.getElementById('width-input');
const heightInput = document.getElementById('height-input');
const analyzeBtn = document.getElementById('analyze-btn');
const loading = document.getElementById('loading');
const error = document.getElementById('error');
const results = document.getElementById('results');
const metadata = document.getElementById('metadata');
const visualizations = document.getElementById('visualizations');

// State
let allTiles = [];
let currentSearchQuery = '';
let currentFilters = { region: '', category: '', country: '' };

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    // Set default date to a known good Sentinel-2 date
    const defaultDate = new Date('2024-10-15');
    dateInput.value = defaultDate.toISOString().split('T')[0];
    dateInput.max = new Date().toISOString().split('T')[0];
    
    // Load data
    loadFilters();
    loadTiles();
    
    // Mode switching
    modeReal.addEventListener('change', () => {
        if (modeReal.checked) {
            realControls.style.display = 'block';
            syntheticControls.style.display = 'none';
        }
    });
    
    modeSynthetic.addEventListener('change', () => {
        if (modeSynthetic.checked) {
            realControls.style.display = 'none';
            syntheticControls.style.display = 'block';
        }
    });
    
    // Search and filter
    let searchTimeout;
    tileSearch.addEventListener('input', () => {
        clearTimeout(searchTimeout);
        searchTimeout = setTimeout(() => {
            currentSearchQuery = tileSearch.value;
            loadTiles();
        }, 300);
    });
    
    regionFilter.addEventListener('change', () => {
        currentFilters.region = regionFilter.value;
        currentFilters.category = '';
        currentFilters.country = '';
        categoryFilter.value = '';
        countryFilter.value = '';
        loadTiles();
    });
    
    categoryFilter.addEventListener('change', () => {
        currentFilters.category = categoryFilter.value;
        currentFilters.region = '';
        currentFilters.country = '';
        regionFilter.value = '';
        countryFilter.value = '';
        loadTiles();
    });
    
    countryFilter.addEventListener('change', () => {
        currentFilters.country = countryFilter.value;
        currentFilters.region = '';
        currentFilters.category = '';
        regionFilter.value = '';
        categoryFilter.value = '';
        loadTiles();
    });
    
    // Tile selection
    tileSelect.addEventListener('change', () => {
        showTileDetails();
        if (modeReal.checked && tileSelect.value) {
            loadAvailableDates(tileSelect.value);
        }
    });
    
    // Load dates when switching to real mode
    modeReal.addEventListener('change', () => {
        if (modeReal.checked && tileSelect.value) {
            loadAvailableDates(tileSelect.value);
        }
    });
    
    // Analyze button
    analyzeBtn.addEventListener('click', runAnalysis);
});

async function loadAvailableDates(tile) {
    const availableDatesDiv = document.getElementById('available-dates');
    const datesList = document.getElementById('available-dates-list');
    
    if (!tile || !availableDatesDiv || !datesList) return;
    
    availableDatesDiv.style.display = 'block';
    datesList.innerHTML = 'Loading available dates...';
    
    try {
        const response = await fetch(`/api/available-dates?tile=${tile}`);
        const data = await response.json();
        
        if (data.error) {
            datesList.innerHTML = `<p class="error">Error: ${data.error}</p>`;
            return;
        }
        
        if (!data.products || data.products.length === 0) {
            datesList.innerHTML = '<p>No products found for this tile</p>';
            return;
        }
        
        // Display available dates
        datesList.innerHTML = '<ul class="dates-list">' +
            data.products.slice(0, 10).map(p => 
                `<li>
                    <button class="date-btn" data-date="${p.date}">${p.date}</button>
                    <span class="product-info">${p.size_mb} MB ${p.online ? '✓ Online' : '⚠ Offline'}</span>
                </li>`
            ).join('') +
            '</ul>';
        
        // Add click handlers to date buttons
        document.querySelectorAll('.date-btn').forEach(btn => {
            btn.addEventListener('click', function() {
                dateInput.value = this.dataset.date;
            });
        });
    } catch (error) {
        console.error('Error loading available dates:', error);
        datesList.innerHTML = `<p class="error">Failed to load dates: ${error.message}</p>`;
    }
}

async function loadFilters() {
    try {
        const [regions, categories, countries] = await Promise.all([
            fetch('/api/regions').then(r => r.json()),
            fetch('/api/categories').then(r => r.json()),
            fetch('/api/countries').then(r => r.json())
        ]);
        
        regionFilter.innerHTML = '<option value="">All Regions</option>' +
            regions.map(r => `<option value="${r}">${r}</option>`).join('');
        
        categoryFilter.innerHTML = '<option value="">All Categories</option>' +
            categories.map(c => `<option value="${c}">${c}</option>`).join('');
        
        countryFilter.innerHTML = '<option value="">All Countries</option>' +
            countries.map(c => `<option value="${c}">${c}</option>`).join('');
    } catch (err) {
        console.error('Failed to load filters:', err);
    }
}

async function loadTiles() {
    try {
        let url = '/api/tiles';
        const params = new URLSearchParams();
        
        if (currentSearchQuery) {
            params.append('q', currentSearchQuery);
        } else if (currentFilters.region) {
            params.append('region', currentFilters.region);
        } else if (currentFilters.category) {
            params.append('category', currentFilters.category);
        } else if (currentFilters.country) {
            params.append('country', currentFilters.country);
        }
        
        if (params.toString()) {
            url += '?' + params.toString();
        }
        
        const response = await fetch(url);
        const tiles = await response.json();
        
        allTiles = tiles;
        tileCount.textContent = tiles.length;
        
        if (tiles.length === 0) {
            tileSelect.innerHTML = '<option value="">No tiles found</option>';
            return;
        }
        
        tileSelect.innerHTML = tiles.map(tile => 
            `<option value="${tile.id}" data-tile='${JSON.stringify(tile)}'>
                ${tile.id} - ${tile.name}, ${tile.country}
            </option>`
        ).join('');
        
        // Auto-select first tile if none selected
        if (tileSelect.selectedIndex === -1 && tiles.length > 0) {
            tileSelect.selectedIndex = 0;
            showTileDetails();
        }
    } catch (err) {
        console.error('Failed to load tiles:', err);
        tileSelect.innerHTML = '<option value="">Error loading tiles</option>';
    }
}

function showTileDetails() {
    const selectedOption = tileSelect.options[tileSelect.selectedIndex];
    if (!selectedOption || !selectedOption.dataset.tile) {
        tileDetails.style.display = 'none';
        return;
    }
    
    try {
        const tile = JSON.parse(selectedOption.dataset.tile);
        
        document.getElementById('tile-detail-name').textContent = tile.name;
        document.getElementById('tile-detail-description').textContent = tile.description;
        document.getElementById('tile-detail-category').textContent = `📍 ${tile.category}`;
        document.getElementById('tile-detail-country').textContent = `🌍 ${tile.country} (${tile.region})`;
        
        tileDetails.style.display = 'block';
    } catch (err) {
        console.error('Error displaying tile details:', err);
        tileDetails.style.display = 'none';
    }
}

async function runAnalysis() {
    // Hide previous results and errors
    error.style.display = 'none';
    results.style.display = 'none';
    
    // Get selected indices
    const indices = Array.from(document.querySelectorAll('input[name="index"]:checked'))
        .map(cb => cb.value);
    
    if (indices.length === 0) {
        showError('Please select at least one spectral index to compute.');
        return;
    }
    
    // Disable button and show loading
    analyzeBtn.disabled = true;
    loading.style.display = 'block';
    
    try {
        let response;
        
        if (modeReal.checked) {
            // Real data mode
            const tileId = tileSelect.value;
            const date = dateInput.value;
            
            if (!tileId || !date) {
                throw new Error('Please select a tile and date.');
            }
            
            response = await fetch('/api/analyze', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ tileId, date, indices })
            });
        } else {
            // Synthetic data mode
            const width = parseInt(widthInput.value);
            const height = parseInt(heightInput.value);
            
            if (width < 64 || height < 64 || width > 4096 || height > 4096) {
                throw new Error('Image dimensions must be between 64 and 4096.');
            }
            
            response = await fetch('/api/analyze-synthetic', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ width, height, indices })
            });
        }
        
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || 'Analysis failed');
        }
        
        const data = await response.json();
        displayResults(data);
        
    } catch (err) {
        console.error('Analysis error:', err);
        showError(err.message);
    } finally {
        analyzeBtn.disabled = false;
        loading.style.display = 'none';
    }
}

function showError(message) {
    error.textContent = message;
    error.style.display = 'block';
    error.scrollIntoView({ behavior: 'smooth', block: 'center' });
}

function displayResults(data) {
    // Display metadata
    metadata.innerHTML = `
        <h3>Scene Information</h3>
        <div class="metadata-item">
            <span class="metadata-label">Tile ID:</span>
            <span class="metadata-value">${data.tileId}</span>
        </div>
        <div class="metadata-item">
            <span class="metadata-label">Date:</span>
            <span class="metadata-value">${data.date}</span>
        </div>
        <div class="metadata-item">
            <span class="metadata-label">Dimensions:</span>
            <span class="metadata-value">${data.width} × ${data.height} pixels</span>
        </div>
        <div class="metadata-item">
            <span class="metadata-label">Total Pixels:</span>
            <span class="metadata-value">${(data.width * data.height).toLocaleString()}</span>
        </div>
        <div class="metadata-item">
            <span class="metadata-label">Indices Computed:</span>
            <span class="metadata-value">${Object.keys(data.indices).length}</span>
        </div>
    `;
    
    // Display visualizations
    visualizations.innerHTML = Object.entries(data.indices).map(([name, index]) => `
        <div class="index-card">
            <h3>${getIndexFullName(name)}</h3>
            <img src="${index.imageUrl}" alt="${name}">
            <div class="index-stats">
                <div class="stat-row">
                    <span class="stat-label">Range:</span>
                    <span class="stat-value">[${index.min.toFixed(3)}, ${index.max.toFixed(3)}]</span>
                </div>
                <div class="stat-row">
                    <span class="stat-label">Mean:</span>
                    <span class="stat-value">${index.mean.toFixed(3)}</span>
                </div>
                <div class="stat-row">
                    <span class="stat-label">Std Dev:</span>
                    <span class="stat-value">${index.stdDev.toFixed(3)}</span>
                </div>
            </div>
        </div>
    `).join('');
    
    // Show results and scroll to them
    results.style.display = 'block';
    results.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

function getIndexFullName(shortName) {
    const names = {
        'NDVI': 'NDVI - Normalized Difference Vegetation Index',
        'EVI': 'EVI - Enhanced Vegetation Index',
        'NDWI': 'NDWI - Normalized Difference Water Index',
        'SAVI': 'SAVI - Soil-Adjusted Vegetation Index',
        'NBR': 'NBR - Normalized Burn Ratio'
    };
    return names[shortName] || shortName;
}

