$.fn.dataTable.ext.search.push(
    function( settings, data, dataIndex ) {
        var minEl = document.getElementById('year-min');
        var maxEl = document.getElementById('year-max');
        
        if (!minEl || !maxEl) return true;
        
        var min = parseInt(minEl.innerHTML, 10);
        var max = parseInt(maxEl.innerHTML, 10);
        var year = parseInt(data[2], 10) || 0; 

        if ( ( isNaN( min ) && isNaN( max ) ) ||
             ( isNaN( min ) && year <= max ) ||
             ( min <= year   && isNaN( max ) ) ||
             ( min <= year   && year <= max ) )
        {
            return true;
        }
        return false;
    }
);

document.addEventListener("DOMContentLoaded", function () {
  const parameters = new URLSearchParams(window.location.search);
  const city = decodeURIComponent(window.location.search.substring(1));

  const title = document.getElementById("city-title");
  const status = document.getElementById("status");

  if (!city) {
    title.textContent = "City not specified";
    return;
  }

  title.textContent = `Publications from ${city}`;
  document.title = `Publications from ${city}`;

  Papa.parse("./intermediates/final_dat.csv", {
    download: true,
    header: true,
    skipEmptyLines: true,

    complete: function (results) {
      const publications = results.data.filter(function (row) {
        return row.city === city;
      });

      if (publications.length === 0) {
        status.textContent = `No studies found for ${city}.`;
        return;
      }

      status.textContent = `${publications.length} publication${publications.length === 1 ? "" : "s"}`;

      new DataTable("#publications", {
        data: publications,
        pageLength: 50,
        dom: "Bfrtip",
        buttons: ["copy", "csv"],
        orderCellsTop: true,
        order: [[2, "desc"]],

        columns: [
          {
            data: "ID",
            defaultContent: "",
            render: function(data, type, row) {
                if (type === 'display' && data) {
                    return `<a href="publications/${data}.html" target="_blank">${data}</a>`;
                }
                return data;
            }
          },
          {
            data: "Ticks",
            defaultContent: ""
          },
          {
            data: "year",
            defaultContent: ""
          },
          {
            data: "ArticleTitle",
            defaultContent: ""
          },
          {
            data: "city",
            defaultContent: "",
            render: function(data, type, row) {
                if (type === 'display' && data) {
                    return `<strong>${data}</strong>`;
                }
                return data;
            }
          }
        ],

        initComplete: function () {
            var api = this.api();

            api.columns().every(function () {
                var column = this;
                var colIdx = column.index();
                var title = $('#publications thead tr:eq(0) th').eq(colIdx).text();
                var cell = $('.filters th').eq(colIdx);

                if (colIdx === 1) {
                    var select = $('<select class="form-select form-select-sm"><option value="">All</option></select>')
                        .appendTo(cell.empty())
                        .on('change', function () {
                            var val = $.fn.dataTable.util.escapeRegex($(this).val());
                            column.search(val ? '^' + val + '$' : '', true, false).draw();
                        });

                    column.data().unique().sort().each(function (d, j) {
                        if(d) select.append('<option value="' + d + '">' + d + '</option>');
                    });
                } 

                else if (colIdx === 2) {
                    var years = publications.map(p => parseInt(p.year, 10)).filter(y => !isNaN(y));
                    var minYear = years.length > 0 ? Math.min(...years) : 2000;
                    var maxYear = years.length > 0 ? Math.max(...years) : new Date().getFullYear();

                    if (minYear === maxYear) {
                         cell.html('<div style="text-align:center; font-size: 13px; font-weight: normal; padding-top: 5px;">' + minYear + '</div>');
                    } else {
                        cell.html(
                            '<div style="padding: 0 10px;">' +
                                '<div id="year-slider" style="margin-top: 10px; margin-bottom: 8px;"></div>' +
                                '<div style="text-align:center; font-size: 12px; font-weight: normal;">' +
                                    '<span id="year-min">' + minYear + '</span> - <span id="year-max">' + maxYear + '</span>' +
                                '</div>' +
                            '</div>'
                        );
                        
                        var $slider = $('#year-slider');
                        
                        // Initialize using the older v7 jQuery syntax
                        $slider.noUiSlider({
                            start: [minYear, maxYear],
                            connect: true,
                            step: 1,
                            range: {
                                'min': minYear,
                                'max': maxYear
                            }
                        });

                        // In v7, we use jQuery event listeners ('slide') instead of 'update'
                        $slider.on('slide', function () {
                            var values = $(this).val(); // Returns an array of strings e.g., ["2010.00", "2020.00"]
                            document.getElementById('year-min').innerHTML = parseInt(values[0], 10);
                            document.getElementById('year-max').innerHTML = parseInt(values[1], 10);
                            api.draw(); 
                        });
                    }
                }

                else {
                    cell.html('<input type="text" placeholder="Filter ' + title + '" class="form-control form-control-sm" />');
                    
                    $('input', cell).off('keyup change').on('keyup change', function (e) {
                        e.stopPropagation();
                        $(this).attr('title', $(this).val());
                        var regexr = '({search})';
                        var cursorPosition = this.selectionStart;
                        column.search(
                            this.value != '' ? regexr.replace('{search}', '(((' + this.value + ')))') : '',
                            this.value != '',
                            this.value == ''
                        ).draw();
                        $(this).focus()[0].setSelectionRange(cursorPosition, cursorPosition);
                    });
                }
            });
        }
      });
    }
  });
});
