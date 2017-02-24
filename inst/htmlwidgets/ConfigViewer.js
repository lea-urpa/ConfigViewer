HTMLWidgets.widget({

  name: 'ConfigViewer',

  type: 'output',

  factory: function(el, width, height) {
	  
	// Shared data
    var scaling = {};
	var rsids = [];
	var datasetnames = [];
	var plotdata = {};
	var domains = {};
	var datasets = {};
	var configs = [];
	
	// Shared path variables
	var inclconfigs = 0;	
	var configbuttons;
	var labels;
	 
	// Shared window size variables
	var plotwidth;
	var plotheight;
	var sidebarwidth;
	
	var windowwidth = width;
    var windowheight = height; 
	
    var padding = 10;
    var margin = {left: 10, right: 10, top: 10, bottom: 10, 
 	   				axis: 40, legends:20 };
	
    return {

      renderValue: function(widgetdata) {
			
			// Extract specific datasets from data object
		    rsids = widgetdata['allplots']['rsids']
			datasetnames = widgetdata['datasetinfo']['datasetnames']
			var positions = widgetdata['allplots']['positions']
		    labels = widgetdata['labels']
		  	var plotlabels = widgetdata['datasetinfo']['plotlabels']
		  
			// Initial dataset path variables
			var inclcorrs = 0;
			configbuttons = 0;
			
		  	// Set plot graphics parameter
			plotwidth = windowwidth - margin.left - margin.right - margin.axis - margin.legends; 
			plotheight = windowheight/datasetnames.length - (margin.legends)*3 
			sidebarwidth = 160;
		  	
			// Evaluate percentage of screen sidebar takes up
			var sidebarpercent = (sidebarwidth/windowwidth * 100) + '%'
			var mainchartpercent = ((windowwidth - sidebarwidth) / windowwidth * 100) + '%'
			
			// Finding min and max values for scaling factors
			domains = { x: {min: Math.min.apply(null, positions), max: Math.max.apply(null, positions)} };
			
			for(var i=0; i<datasetnames.length; i++){
				domains[datasetnames[i]] = {
				   min: Math.min.apply(null, widgetdata['plotdata'][datasetnames[i]] ),
				   max: Math.max.apply(null, widgetdata['plotdata'][datasetnames[i]] )
				}
			}
			
		    // Create scaling factors
		    scaling = {
		 	   x: d3.scaleLinear()
		    			 .domain([domains.x.min, domains.x.max])
		   	 		 .range([0,plotwidth]),
		 	   x_sidebar: d3.scaleLinear()
		    			 .domain([domains.x.min, domains.x.max])
		   	 		 .range([0,(plotwidth-sidebarwidth)])
		    }
			
		    for(var i=0; i<datasetnames.length; i++){
		 	   scaling[datasetnames[i]] = 
		 		   (d3.scaleLinear()
		 	 	  		.domain([ domains[datasetnames[i]].max, domains[datasetnames[i]].min ])
		    	 			.range([ 0, (plotheight - margin.axis)])) ;
		    }
			
			// Indexable object by dataset name, for dataset-wide info
			for(var i=0; i< datasetnames.length; i++){
				datasets[datasetnames[i]] = {
					threshold: widgetdata['datasetinfo']['thresholds'][i],
					labelprefix: widgetdata['datasetinfo']['labelprefix'][i],
					yaxislabel: widgetdata['datasetinfo']['yaxislabels'][i],
					plotlabel: widgetdata['datasetinfo']['plotlabels'][i]
				}
			}
			
			// If corr/config files given, create objects and update path variables
			var configprobs = [];
			
			if( Object.keys(widgetdata).includes('configdata') == true ){
				configs = widgetdata['configdata']['configs']
				configprobs = widgetdata['configdata']['configprobs']
	
				inclconfigs = 1
			}
 		    
			if( Object.keys(widgetdata['allplots']).includes('correlations') == true ){					
				inclcorrs = 1
			}

		    // Indexable object by rsid, by dataset name
		    for(var i=0; i< datasetnames.length; i++){
		 	   temp_obj = {};
		 	   for(var j=0; j<rsids.length; j++){
		 		  temp_obj[rsids[j]] = {
		 			  plotdata: widgetdata['plotdata'][datasetnames[i]][j],
		 			  labeldata: widgetdata['labeldata'][datasetnames[i]][j],
		 			  threshcolor: widgetdata['threshcolors'][datasetnames[i]][j],
		 			  position: positions[j]
		 		   }
		 	   }
		 	   plotdata[datasetnames[i]] = temp_obj;
		    }

			// Separate rsid indexable object for corr data, if it exists
			var corrdata = {};
			if(inclcorrs == 1 ){
				for(var i=0; i<rsids.length; i++){
					corrdata[rsids[i]] ={
						topsnpcorr: widgetdata['allplots']['correlations'][i],
						corrcolor: widgetdata['allplots']['corrcolors'][i]
					}
				}
			} else {
				for(var i=0; i<rsids.length; i++){
					corrdata[rsids[i]] ={
						topsnpcorr: "NA",
						corrcolor: "NA"
					}
				}
			}
			

			// Separate rsid indexable object for config data, if it exists
			var rankdata = {};
			if(inclconfigs == 1){
				for(var i=0; i<rsids.length; i++){
					rankdata[rsids[i]] ={
						rank:  widgetdata['allplots']['configranks'][i]
					}
				}
			} else {
				for(var i=0; i<rsids.length; i++){
					rankdata[rsids[i]] = {
						rank:  "NA"
					}
				}
			}

			// Create first div for buttons
			d3.select(el)
				.append('div')
				.attr('id','buttondiv')
					.append('svg')
						.attr('id', 'buttonsvg')
						.attr('width', 700) // Arbitrarily large to always show buttons + text
						.attr('height', margin.legends + padding/2)
	
			// Create second div for charts
			d3.select(el)
				.append('div')
				.attr('id', 'plotsdiv')
					.append('table')
					.attr('id', 'plotstable')
	
		    // Variables holding iterable values and html data 
			var rows = [];
			var plots = [];
		    var chart = [];
		    var main = [];
		    var textback = [];
		    var textitself = [];
		    var backgroundrects = [];
			
		    // Add the objects that are doubled/tripled depending on how many datasets you want to input
		    for(var j=0; j<datasetnames.length; j++){
	   
			   var dat = datasetnames[j]
		
			   // Add table row and table data
			   rows[j] = d3.select('#plotstable')
							.append('tr')
							.attr('id', 'row'+j)
					   
			   plots[j] = rows[j].append('td')
									.attr('width', '100%')
	   
			   // Add svg
		 	   chart[j] = plots[j].append('svg:svg')
		 					.attr('id', 'chart'+j)
		 					.attr('width', windowwidth)
		 					.attr('height', windowheight/datasetnames.length - margin.legends*3)
		 					.attr('class', 'chart');
	   
		 	   // Add threshold line
		 	   chart[j].append('line')
		 			.attr('class', 'line')
		 	  		.attr('transform', 'translate(' + (margin.axis + margin.legends) + ',' + margin.top + ')')
		 			.attr('y1', scaling[dat]( datasets[dat]['threshold'] ))
		 	  		.attr('y2', scaling[dat]( datasets[dat]['threshold'] ))
		 	 		.attr('x1', scaling.x(domains.x.min) )
		 	   		.attr('x2', scaling.x(domains.x.max) )
		 	   		.style('stroke', 'grey' )
		 	   		.style('stroke-windowwidth', 1)
	   
		 	   // Add main scatterplot
		 	   main[j] = chart[j].append('g')
		 			.attr('transform', 'translate(' + (margin.axis + margin.legends) + ',' + margin.top + ')')
		 	   		.attr('width', plotwidth)
		 	   		.attr('height', plotheight)
		 	   		.attr('class', 'main')
		 	   		.attr('id', 'main'+j);
	   
		 	   main[j].append('svg:g')
		 			.selectAll('circle')
		 			.data(rsids)
		 			.enter().append('circle')
		 				.attr('cx', function(d,i){ return scaling.x( plotdata[dat][d]['position'] ); })
		 				.attr('cy', function(d){ return scaling[dat]( plotdata[dat][d]['plotdata'] ); })
		 				.attr('r', 4)
		 				.attr('id', function(d){ return d; })
		 				.attr('class', function(d){ return d + ' '+ dat + ' ' + corrdata[d]['corrcolor'] 
		 													+ ' ' + rankdata[d]['rank']  })
		 				.attr('fill', function(d,i){ return( plotdata[dat][d]['threshcolor']); })
		 					.on('mouseover', function(d){
		 						if(configbuttons  == 0){
		 							d3.selectAll('circle')
		 								.style('fill-opacity', 0.05)
		 							d3.selectAll('.' + d )
		 								.style('fill-opacity', 1)	
		 						};
		 						d3.selectAll('.'+d+'label')
		 							.style('visibility','visible')
		 					})
		 					.on('mouseout', function(d){
		 						if(configbuttons == 0){
		 							d3.selectAll('circle')
		 								.style('fill-opacity', 1)
		 								.style('stroke-opacity', 0)
		 						};
		 						d3.selectAll('.'+d+'label')
		 							.style('visibility', 'hidden')
		 					})
					
		 	    // svgs for text labels and backing, created in correct z order
		 	    textback[j] = main[j].append('svg:g')
		 					.attr('id', 'textback'+j)
		
		 		textitself[j] = main[j].append('svg:g')
		 					.attr('id', 'textitself'+j)
		
		 		// Add text labels
		 		textitself[j].selectAll('text')
		 				.data(rsids)
		 				.enter()
		 				.append('text')
		 					.attr('class', function(d){ return d +'label'; })
		 					.attr('y', function(d,i) {return scaling[dat](plotdata[dat][d]['plotdata']) ; })
		 					.text( function(d,i){ return (d + ', '+ datasets[dat]['labelprefix'] + 
		 													plotdata[dat][d]['labeldata']) })
		 					.attr('font-family', 'sans-serif')
		 					.attr('font-size', '12px')
		 					.style('visibility', 'hidden')
		 					.style('pointer-events', 'none')
					
				// Create object of rsids with empty attr array
		 		backgroundrects[j] = d3.range(rsids.length).map(function(d,i){
		 			return { attrs: [] }
		 		})
		
				// Fill attr array with BBox info for corresponding rsid
		 		main[j].selectAll('text').each(function(d,i){
		 			backgroundrects[j][i].attrs = this.getBBox();
		 		})
		
				// Fix text such that those on the right anchor at the end, so labels don't overflow
		 		d3.select('#textitself'+j).selectAll('text')
		 					.style('text-anchor', function(d,i){
		 						if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
		 							return 'end';
		 						} else { return 'start'; }
		 					})
		 					.attr('x', function(d,i){
		 						if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
		 							return scaling.x(plotdata[dat][d]['position']) - 5;
		 						} else { return scaling.x(plotdata[dat][d]['position']) + 5;}
		 					})
		
				// Create background rectangles using BBox data for each rsid
		 		textback[j].selectAll('rect')
		 			.data(backgroundrects[j])
		 				.enter().append('rect')
		 		   			.attr('class', function(d,i){ return rsids[i]+'label' })
							.attr('x', function(d,i){ 
								if( scaling.x(plotdata[dat][rsids[i]]['position'])  > plotwidth/2){
									return scaling.x(plotdata[dat][rsids[i]]['position']) - backgroundrects[j][i].attrs.width - 5;
								} else { return scaling.x(plotdata[dat][rsids[i]]['position']) + 5;}
							})
		 		  	 		.attr('y', function(d,i){ return d.attrs.y })
		 		  	 		.attr('height', function(d,i){ return d.attrs.height })
		 		  	 		.attr('width', function(d,i){ return d.attrs.width })
		 					.attr('fill', 'white')
		 		   			.style('visibility', 'hidden')
		 		   			.attr('rx', 2)
		 		   			.attr('ry', 2)
		
		 		// Add left axis			
		 		chart[j].append('g')
		 		   .attr('transform','translate(' + (margin.axis + margin.legends - padding) + ',' + margin.top + ')')
		 		   .attr('class', 'yaxis')
		 		 	   .call(d3.axisLeft(scaling[dat])
		 		  			.ticks(10));
	    
		 		// Add left axis label
		 		chart[j].append('text')
		 		   .attr('transform', 'rotate(-90)')
				   .attr('class', 'yaxislabel')
		 		   .attr('x', -margin.top - plotheight/2)
		 		   .attr('y', margin.left - padding)
		 		   .attr('dy', '1em')
		 		   .style('text-anchor', 'middle')
		 		   .text(datasets[dat]['yaxislabel'])
		
		 		// Add bottom axis
		 		chart[j].append('g')
		 			.attr('transform', 'translate(' + (margin.axis + margin.legends) + ',' + (margin.top + plotheight - margin.axis + padding) + ')')
		 		    .attr('class', 'xaxis')
		 			 	 .call(d3.axisBottom(scaling.x)
		 		  			.ticks(10)
		 					.tickFormat(d3.formatPrefix(".1", 1e6)))
							
				// Add ID tag for datasets in the top left of plot area
				if(labels == true | datasetnames.includes('condlogpval')){
					d3.select('#chart'+j)
						.append('text')
						.attr('x', margin.axis + margin.legends)
						.attr('y', margin.top)
						.text(datasets[dat]['plotlabel'] )	
				}
		    }
			
			
			// Add div for bottom axis label
			var axisdiv = d3.select(el)
								.append('div');

			var axissvg = axisdiv.append('svg:svg')
					.attr('id', 'axissvg')
					.attr('width', windowwidth)
					.attr('height', margin.legends + padding/2)

			// Add bottom axis label
			axissvg.append('text')
			   .attr('y',0)
			   .attr('x', plotwidth/2 + margin.axis + margin.left)
			   .attr('id', 'xaxislabel')
			   .attr('dy', '1em')
			   .style('text-anchor', 'middle')
			   .text('Position (Megabases)')	


			// Add div for legends
			var legenddiv = d3.select(el)
								.append('div');

			var legendsvg = legenddiv.append('svg:svg')
					.attr('id', 'legendsvg')
					.attr('width', 700)
					.attr('height', margin.legends + padding/2)
			
			
			// Add button/color legend for coloring change

			// Add group for button text
			var buttonsgroup = d3.select('#buttonsvg')
									.append('g')
			  						.attr('id', 'buttonsgroup')

			var corrbuttonwidth = 120
			var corrbuttonheight = 20

			buttonsgroup.append('text')
				.attr('transform', 'translate('+ (margin.left + margin.axis + padding + corrbuttonwidth/2) 
										+ ',' + (margin.top + corrbuttonheight/4 + padding/4) + ')')
				.attr('id', 'corrtext')
				.text('Correlation colors')
				.style('text-anchor', 'middle')

			var path = 1;

			buttonsgroup.append('rect')
				.data(rsids)
					.attr('id', 'corrbutton')
					.attr('width', corrbuttonwidth)
					.attr('height', corrbuttonheight)
					.attr('x', margin.left + margin.axis + padding)
					.attr('y', padding/4)
					.attr('rx', 5)
					.attr('ry', 5)
					.attr('fill-opacity', '0')
					.attr('stroke', 'grey')
						.on('click', function(){
							if(inclcorrs == 1){
							if(path == 1) {
								d3.selectAll('.navy')
									.attr('fill', 'navy')
								d3.selectAll('.deepskyblue')
									.attr('fill', 'deepskyblue')
								d3.selectAll('.limegreen')
									.attr('fill', 'limegreen')
								d3.selectAll('.orange')
									.attr('fill', 'orange')
								d3.selectAll('.firebrick')
									.attr('fill', 'firebrick')
								d3.select('#corrtext')
									.transition()
									.text('Significance colors')	

								d3.selectAll('.colorlegend')
									.style('visibility', 'visible')

								d3.selectAll('.threshlegend')
									.style('visibility', 'hidden')

								path = 0

							} else {
								d3.select('#corrtext')
									.text('Correlation colors')
								d3.selectAll('.logBF')
									.attr('fill', function(d,i){ return(plotdata['logBF'][d]['threshcolor']); })
								d3.selectAll('.logpval')
									.attr('fill', function(d,i){ return(plotdata['logpval'][d]['threshcolor']); })
								d3.selectAll('.condlogpval')
									.attr('fill', function(d,i){ return(plotdata['condlogpval'][d]['threshcolor']); })
								d3.selectAll('.colorlegend')
									.style('visibility', 'hidden')

								d3.selectAll('.threshlegend')
									.style('visibility', 'visible')

								if (configbuttons == 0){
									d3.selectAll('circles')
										.style('fill-opacity', 1)
								}

								path = 1
							}
							}	
						})

	
			// Add buttons for top configs
			var buttonnumbers = [];
			if(inclconfigs == 1){
				for(var i =1; i <= configs.length; i++){
					buttonnumbers.push(i);
				}
			} else {
				buttonnumbers = [1,2,3,4,5];
			}

			var buttonsize = 20;
			var buttonpadding = 5;

			d3.select('#buttonsgroup').selectAll('rect')
						.data(buttonnumbers, function(d,i) { return d + i;}) // I have no idea why this works but it fixed missing button 1
							.enter().append('rect')
							.attr('id', function(d){ return 'config' + d })
							.attr('height', buttonsize)
							.attr('width', buttonsize)
							.attr('x', function(d){ return (margin.left + margin.axis + padding + 
									corrbuttonwidth + buttonpadding + (buttonsize + buttonpadding)*(d - 1)) })
							.attr('y', padding/4)
							.attr('rx', 5)
							.attr('ry', 5)
							.attr('fill-opacity', 0)
							.attr('stroke', 'grey')
								.on('click', function(d){
									if(inclconfigs == 1){
									// Change opacities of circles to highlight top rsids
									d3.selectAll('circle')
										.style('fill-opacity', 0.05)
										.style('stroke', 'none' )
									d3.selectAll('.rank'+d)
										.style('fill-opacity', 1)
										.style('stroke', 'red')
										.style('stroke-opacity', 1)
										.style('stroke-windowwidth', 1.5)
									// Make charts visible
									d3.selectAll('.configspecific')
										.style('visibility', 'hidden')
									d3.selectAll('#outerpiechart, #path'+d+', #arctext'+d+', #piechartlabel, #rsidlist'+d)
										.style('visibility', 'visible')
									// Remove any previous list items
									d3.selectAll('li').remove()
									// Create new rsid list
									d3.select('#rsidlist'+d).selectAll('li')
										.data(configs[d-1])
										.enter().append('li')
										.style('font-size', '12px')
										.style('font-family', 'sans-serif')
											.append('a')
											.attr('href',function(d,i){return 'https://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=' +d })
											.html(String)
		
		
									if(configbuttons == 0){ // If config buttons have not been clicked before
			
										// Remove x axes
										d3.selectAll('.xaxis').remove()
										// Change td and svg to correct widths
										d3.select('#configsubset')
												.attr('width', sidebarpercent)
										d3.selectAll('.chart')
												.attr('width', windowwidth - sidebarwidth)
										// Change threshold line length
										d3.selectAll('.line')
											.attr('x2', scaling.x_sidebar(domains.x.max))
			
										// Variables that are different per dataset
										for(var j=0; j<datasetnames.length; j++){
											var dat = datasetnames[j]; 
				
											// Update circle locations
											d3.selectAll('.chart').selectAll('circle')
												.data(rsids)
												.transition()
												.attr('cx', function(d,i){ return scaling.x_sidebar(plotdata[dat][d]['position']); })
											// Update text locations
											d3.selectAll('.chart').selectAll('text')
												.data(rsids)
												.transition()
												.attr('x', function(d,i){ 
													if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
														return scaling.x_sidebar(plotdata[dat][d]['position']) - 5;
													} else { return scaling.x_sidebar(plotdata[dat][d]['position']) + 5;}
											})
											// Update background rectangle locations
											d3.selectAll('#chart'+j).selectAll('rect')
												.data(rsids)
												.transition()
												.attr('x', function(d,i){ 
													if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
														return scaling.x_sidebar(plotdata[dat][d]['position']) - backgroundrects[j][i].attrs.width - 5;
													} else { return scaling.x_sidebar(plotdata[dat][d]['position']) + 5;}
												})
											// Recreate x axes
											d3.select('#chart'+j).append('g')
												.attr('transform', 'translate(' + (margin.axis + margin.legends) + 
														',' + (margin.top + plotheight - margin.axis + padding) + ')')
											    .attr('class', 'xaxis')
												 	 .call(d3.axisBottom(scaling.x_sidebar)
											  			.ticks(10)
														.tickFormat(d3.formatPrefix(".1", 1e6)));
										}
									}
									configbuttons = 1 // Set path variable to show a button has been clicked
									}
								})
								.on('dblclick', function(d){
									if(inclconfigs == 1){
									if(configbuttons == 1){ // Only do the stuff if the button has been clicked before
										// Change circle opacities back to normal
										d3.selectAll('circle')
											.style('fill-opacity', 1)
											.style('stroke-opacity', 0)
										// Change td and svg to correct widths
										d3.select('#configsubset')
												.attr('width', '0%')
										d3.selectAll('.chart')
												.attr('width', windowwidth)
										// Hide charts again
										d3.selectAll('#outerpiechart, #path'+d+', #arctext'+d+', #piechartlabel, #rsidlist'+d)
											.style('visibility', 'hidden')
										// Change threshold line back to normal
										d3.selectAll('.line')
											.attr('x2', scaling.x(domains.x.max) )
										// Remove x axes
										d3.selectAll('.xaxis').remove()
										// Remove list items 
										d3.selectAll('li').remove()
			
										// Variables that are different per dataset
										for(var j=0; j<datasetnames.length; j++){
											var dat = datasetnames[j]; 
											// Update circle locations
											d3.selectAll('.chart').selectAll('circle')
												.data(rsids)
												.transition()
												.attr('cx', function(d,i){ return scaling.x(plotdata[dat][d]['position']); })
											// Update text locations
											d3.selectAll('.chart').selectAll('text')
												.data(rsids)
												.transition()
												.attr('x', function(d,i){
													if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
														return scaling.x(plotdata[dat][d]['position']) - 5;
													} else { return scaling.x(plotdata[dat][d]['position']) + 5;}
											})
											// Update background rect locations
											d3.selectAll('#chart'+j).selectAll('rect')
												.data(rsids)
												.transition()
												.attr('x', function(d,i){ 
													if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
														return scaling.x(plotdata[dat][d]['position']) - backgroundrects[j][i].attrs.width - 5;
													} else { return scaling.x(plotdata[dat][d]['position']) + 5;}
												})
											// Recreate axes
											d3.select('#chart'+j).append('g')
												.attr('transform', 'translate(' + (margin.axis + margin.legends) + 
														',' + (margin.top + plotheight - margin.axis + padding) + ')')
											    .attr('class', 'xaxis')
												 	 .call(d3.axisBottom(scaling.x)
											  			.ticks(10)
														.tickFormat(d3.formatPrefix(".1", 1e6)));
										}
										configbuttons = 0 // Let path variable know buttons have not been clicked
									}
									}
								})
								.on('mouseover', function(){
									if(inclconfigs == 1){
									d3.select('#infotext')
										.style('visibility', 'visible')
									}
								})
								.on('mouseout', function(){
									if(inclconfigs == 1){
									d3.select('#infotext')
										.style('visibility', 'hidden')
									}
								})
			
		   // Add button text
		   d3.select('#buttonsgroup').selectAll('text')
				.data(buttonnumbers, function(d,i) { return d + i;})
				.enter().append('text')
					.attr('id', function(d,i) { return 'config'+ d + 'text'})
					.attr('x', function(d,i){ 
						return (margin.left + margin.axis + padding + corrbuttonwidth +
						(buttonsize + buttonpadding)*(d) - buttonsize/2)
					})
					.attr('y', margin.top + buttonsize/4 + padding/4)
					.text(function(d,i){ return d })
					.style('text-anchor', 'middle')
					.style('pointer-events', 'none')

			d3.select('#buttonsgroup')
					.append('text')
					.attr('id','infotext')
					.attr('x', margin.left + margin.axis + padding + corrbuttonwidth +
								(buttonsize + buttonpadding)*buttonnumbers.length + padding)
					.attr('y', margin.top + buttonsize/4 + padding/4)
					.text('Top configurations, dbl click to exit')
					.style('fill-opacity', 0.5)
					.style('visibility', 'hidden')

			// Threshold color legend
			var threshboxwidth = 90
			var boxwidth = 55
			var boxheight = 20

			var threshcolors = { logBF: 'orange', logpval: 'deepskyblue', condlogpval: 'deepskyblue'}
			var threshbinds = {};

			for( var i= 0; i< datasetnames.length; i++){
				var temptext

				if(datasetnames[i] == 'logBF'){
					temptext = ('logBF > ' + datasets[datasetnames[i]]['threshold'])
				} else {
					temptext = ('pvalue < 1e' + -datasets[datasetnames[i]]['threshold'])
				}
				threshbinds[datasetnames[i]] = temptext
			}

			legendsvg.append('g')
					.attr('id','threshlegendgroup')

			for( var i= 0; i< datasetnames.length; i++){
				dat = datasetnames[i]
				if(datasetnames[i] == 'condlogpval'){
					
				} else {
				d3.select('#threshlegendgroup').append('rect')
					.attr('class', 'threshlegend')
					.attr('id', threshcolors[dat])
					.attr('x', threshboxwidth*i + margin.left + margin.axis + padding)
					.attr('y', 0)
					.attr('height', boxheight)
					.attr('width', threshboxwidth)
					.attr('fill', threshcolors[dat])

				   d3.select('#threshlegendgroup').append('text')
				   		.attr('class', 'threshlegend')
						.attr('x', threshboxwidth*i + threshboxwidth/2 + margin.left + margin.axis + padding)
						.attr('y',  3*boxheight/4 )
						.text(threshbinds[dat])
				   		.attr('fill', 'white')
				   		.style('text-anchor', 'middle')
				 	  	.style('visibility', 'visible')
						.style('pointer-events', 'none')
				}
			}

			// Correlation color legend	
			if(inclcorrs == 1){
				var legendbins = ['0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.0']
				var legendcolors = ['navy', 'deepskyblue', 'limegreen', 'orange', 'firebrick']

				legendsvg.append('g')
						.attr('id', 'colorlegendgroup')

				for(var i= 0; i< legendcolors.length; i++){
					  d3.select('#colorlegendgroup').append('rect')
						.attr('class', 'colorlegend')
				   	 	.attr('id', legendcolors[i])
						.attr('x', boxwidth*i + margin.left + margin.axis + padding)
						.attr('y', 0 )
						.attr('height', boxheight)
						.attr('width', boxwidth)
						.attr('fill', legendcolors[i])
				 		.style('visibility', 'hidden')
							.on('mouseover', function(){
								if(configbuttons == 0){
									d3.selectAll('circle')
										.style('fill-opacity', 0.05)
									d3.selectAll('.'+this.id)
										.style('fill-opacity', 1)
								}
							})
							.on('mouseout', function(){
								if(configbuttons == 0){
									d3.selectAll('circle')
										.style('fill-opacity', 1)
								}
							})

					   d3.select('#colorlegendgroup').append('text')
					   		.attr('class', 'colorlegend')
							.attr('x', boxwidth*i + boxwidth/2 + margin.left + margin.axis + padding)
							.attr('y', 3*boxheight/4 )
							.text(legendbins[i])
					   		.attr('fill', 'white')
					   		.style('text-anchor', 'middle')
					 	  	.style('visibility', 'hidden')
							.style('pointer-events', 'none')
				}

				legendsvg.append('text')
				    .attr('class', 'colorlegend')
					    .attr('x', margin.axis + margin.left + padding*2 + boxwidth*legendbins.length)
					    .attr('y', padding/4 + 3*boxheight/4 )
					    .text('Abs. corr. with top snp')
						.style('text-anchor', 'left')
						.style('visibility', 'hidden')
			}
			
		  	// Add plot for config probability
		  	if(inclconfigs == 1){

				// Create arcs for pie chart filling
				var piechartrad = 30
				var arcs = [];

				for(var i=0; i<buttonnumbers.length; i++){
				 	var arc_temp = d3.arc()
									.outerRadius(piechartrad - 1)
									.innerRadius(0)
									.startAngle(0)
									.endAngle(2 * Math.PI * configprobs[(buttonnumbers[i] - 1)])
					arcs.push(arc_temp)
				}

				configsubset = rows[0].append('td')
									 .attr('id', 'configsubset')
									 .attr('width', '0%')
									 .attr('rowspan', '2')

				configsubset.append('svg')
				   		.attr('id', 'subsetsvg')
						.attr('width', sidebarwidth)
						.attr('height',  piechartrad *3.5)
								.append('g')
				   				.attr('id', 'arcgroup')
				   				.attr('transform', 'translate(' + (piechartrad + padding) + ',' 
														+ (piechartrad + padding) + ')')   	

				 // Add outer circle
				 d3.select('#subsetsvg')
				 		.append('ellipse')
					    .attr('id', 'outerpiechart')
						.attr('class', 'configspecific')
					    .attr('rx', piechartrad )
					 	.attr('ry', piechartrad )
					    .attr('cx', piechartrad + padding)
				 	    .attr('cy', piechartrad + padding)
					    .style('fill', 'none')
					    .style('stroke', 'black')
					    .style('visibility', 'hidden')

				 // Add arcs	
				 d3.select('#arcgroup').selectAll('path')
						.data(buttonnumbers)
								.enter().append('path')
									.attr('id', function(d){ return 'path'+d})
									.attr('class','configspecific')
									.attr('d', function(d){
									return d3.arc()
											.outerRadius(piechartrad - 1)
											.innerRadius(0)
											.startAngle(0)
											.endAngle(2* Math.PI * configprobs[(d-1)])()
									})
								.style('fill', 'firebrick')
								.style('stroke', 'firebrick')
								.style('visibility', 'hidden')

				// Add config probability numbers
				d3.select('#arcgroup').selectAll('text')
					.data(buttonnumbers)
							.enter().append('text')
								.attr('id', function(d){ return 'arctext'+d})
								.attr('class', 'configspecific')
								.attr('transform', function(d){
									return 'translate(' + arcs[(d-1)].centroid() + ')' 
								})
								.attr('dy', '2em')
								.attr('dx', '-2em')
								.text(function(d){
									return Math.round(configprobs[(d-1)] * 100) / 100
								})
								.style('visibility', 'hidden')

				// Create label for pie chart
				d3.select('#subsetsvg')
				   .append('text')
						.attr('class', 'configspecific')
				   		.attr('id', 'piechartlabel')
				   		.attr('x', padding)
						.attr('y',  piechartrad*2 + padding*3)
				   		.text('Config causal prob.')
				   		.style('visibility', 'hidden')

				// Create top RSID lists
				d3.select('#configsubset').selectAll('ul')
						.data(buttonnumbers)
						.enter().append('ul')
						.attr('id', function(d,){ return 'rsidlist'+d})
						.attr('class', 'configspecific')
						.style('visibility', 'hidden')
						.style('padding-left', padding + 'px')				
			}

			// Set font
			d3.selectAll('text')
				.style('font-size', '12px')
				.style('font-family', 'sans-serif')		
			
      },
	  
      resize: function(width, height) {
			windowwidth = width; 
			windowheight = height; 

			// Update plotwidth etc
			plotwidth = windowwidth - margin.left - margin.right - margin.axis - margin.legends; 
			plotheight = windowheight/datasetnames.length - (margin.legends)*3 ;
			sidebarwidth = plotwidth/4;

			// Update ranges for scaling factors
			scaling.x
			 .range([0,plotwidth]);
			scaling.x_sidebar
			 .range([0, (plotwidth-sidebarwidth)]);

			for(var i=0; i<datasetnames.length; i++){
			  scaling[datasetnames[i]]
			  	.range([0, (plotheight - margin.axis)]);
			}

			// Remove all x and y axes
			d3.selectAll('.xaxis').remove()
			d3.selectAll('.yaxis').remove()

			var backgroundrects = [];

			for(var j=0; j<datasetnames.length; j++){
				var dat = datasetnames[j]
			
				// Update aspects that are not dependent on configsbutton path variable
				// Update svg size
				d3.select('#chart'+j)
					.attr('height', windowheight/datasetnames.length - margin.legends*3)
				// Update position of circles in chart
				d3.select('#chart'+j).selectAll('circle')
					.data(rsids)
						.attr('cy', function(d){ return scaling[dat]( plotdata[dat][d]['plotdata'] ); })
				// Update position of threshold line
				d3.select('#chart'+j).selectAll('.line')
					.attr('y1', scaling[dat]( datasets[dat]['threshold'] ))
					.attr('y2', scaling[dat]( datasets[dat]['threshold'] ))
					.attr('x1', scaling.x(domains.x.min) )
				// Update position of text labels
				d3.select('#main'+j).selectAll('text')
					.data(rsids)
						.attr('y', function(d,i) {return scaling[dat](plotdata[dat][d]['plotdata']) ; })
				// Update position of background rectangles
				backgroundrects[j] = d3.range(rsids.length).map(function(d,i){
				return { attrs: [] }
				})

				d3.select('#main'+j).selectAll('text').each(function(d,i){
				backgroundrects[j][i].attrs = this.getBBox();
				})
				
				d3.select('#textback'+j).selectAll('rect')
					.data(backgroundrects[j])
					.attr('y', function(d,i){ return d.attrs.y })
				
				// Redefine y axes
				d3.select('#chart'+j).append('g')
				   .attr('transform','translate(' + (margin.axis + margin.legends - padding) + ',' + margin.top + ')')
				   .attr('class', 'yaxis')
				 	   .call(d3.axisLeft(scaling[dat])
				  			.ticks(10));
				
				// Update things that depend on whether pie chart is visible
				if(configbuttons == 0){
				  	// Updates sizes of svgs
					d3.select('#chart'+j)
						.attr('width', windowwidth)
					// Update position of circles in chart
					d3.select('#chart'+j).selectAll('circle')
						.data(rsids)
							.attr('cx', function(d,i){ return scaling.x( plotdata[dat][d]['position'] ); })
					// Update position of threshold line
					d3.select('#chart'+j).selectAll('.line')
						.attr('x2', scaling.x(domains.x.max) )
					// Update position of text labels
					d3.select('#main'+j).selectAll('text')
						.data(rsids)
							.attr('x', function(d,i){
								if( scaling.x(plotdata[dat][d]['position'])  > plotwidth/2){
									return scaling.x(plotdata[dat][d]['position']) - 5;
								} else { return scaling.x(plotdata[dat][d]['position']) + 5;}
							})
					// Update position of background rectangles		
					d3.select('#textback'+j).selectAll('rect')
						.data(backgroundrects[j])
						.attr('x', function(d,i){ 
							if( scaling.x(plotdata[dat][rsids[i]]['position'])  > plotwidth/2){
								return scaling.x(plotdata[dat][rsids[i]]['position']) - backgroundrects[j][i].attrs.width - 5;
							} else { return scaling.x(plotdata[dat][rsids[i]]['position']) + 5;}
						})
					// Redefine x axes
					d3.select('#chart'+j).append('g')
						.attr('transform', 'translate(' + (margin.axis + margin.legends) + 
								',' + (margin.top + plotheight - margin.axis + padding) + ')')
					    .attr('class', 'xaxis')
						 	 .call(d3.axisBottom(scaling.x)
					  			.ticks(10)
								.tickFormat(d3.formatPrefix(".1", 1e6)));
				} else {
					// Updates sizes of svgs
					d3.select('#chart'+j)
						.attr('width', windowwidth - sidebarwidth)
					// Update position of circles in chart
					d3.select('#chart'+j).selectAll('circle')
						.data(rsids)
							.attr('cx', function(d,i){ return scaling.x_sidebar( plotdata[dat][d]['position'] ); })	
					// Update position of threshold line
					d3.select('#chart'+j).selectAll('.line')
						.attr('x2', scaling.x_sidebar(domains.x.max) )
					// Update position of text labels
					d3.select('#main'+j).selectAll('text')
						.data(rsids)
							.attr('x', function(d,i){
								if( scaling.x_sidebar(plotdata[dat][d]['position'])  > plotwidth/2){
									return scaling.x_sidebar(plotdata[dat][d]['position']) - 5;
								} else { return scaling.x_sidebar(plotdata[dat][d]['position']) + 5;}
							})
					// Update position of background rectangles
					d3.select('#textback'+j).selectAll('rect')
						.data(backgroundrects[j])
						.attr('x', function(d,i){ 
							if( scaling.x_sidebar(plotdata[dat][rsids[i]]['position'])  > plotwidth/2){
								return scaling.x_sidebar(plotdata[dat][rsids[i]]['position']) - backgroundrects[j][i].attrs.width - 5;
							} else { return scaling.x_sidebar(plotdata[dat][rsids[i]]['position']) + 5;}
						})
					// Redefine x axes
					d3.select('#chart'+j).append('g')
						.attr('transform', 'translate(' + (margin.axis + margin.legends) + 
								',' + (margin.top + plotheight - margin.axis + padding) + ')')
					    .attr('class', 'xaxis')
						 	 .call(d3.axisBottom(scaling.x_sidebar)
					  			.ticks(10)
								.tickFormat(d3.formatPrefix(".1", 1e6)));
				}
			}

			// Updated positions of y axis labels
			d3.selectAll('.yaxislabel')
			    .attr('x', -margin.top - plotheight/2)
			  	.attr('y', margin.left - padding)

			// Update position of x axis label
			d3.select('#xaxislabel')
				.attr('x', plotwidth/2 + margin.axis + margin.left)

			// Update position of subsetsvg
			d3.select('#subsetsvg')
				.attr('x', windowwidth - sidebarwidth)

			// Update position of listsvg
			d3.select('#listsvg')
				.attr('x', windowwidth - sidebarwidth)

	      }

    };
  }
});