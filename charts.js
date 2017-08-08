$(function () {
                            

    $(document).ready(function () {	
	
		var initialData=[];
		var anormDetec=[];
     
    $.get('test6.csv', function(data){
      
      
        var lines = data.split('\n');
        for(var i = 1;i<=400;i++){
				   initialData.push({							
   				    x: i,
   				    y: parseFloat(lines[i-1])
					 });
					 anormDetec.push({
					    x: i,
					    y: 0
					 });
        }
    
		
		    getVariableX = function(){
			    var aaa= JSON.parse(dataFromServer);
			    return parseInt(aaa.X)};
	
		    getVariableY = function(){
			    var bbb = JSON.parse(dataFromServer);
			    return bbb.Y*1};
			
				getVariableZ = function(){
			    var ccc = JSON.parse(dataFromServer);
			    return ccc.Z*0.3};
	
	
        Highcharts.setOptions({
            global: {
                useUTC: false,
				pointStart:0
            }
        });

        $('#container').highcharts({
            chart: {
				        pointStart:'0',
                type: 'line',
                animation: Highcharts.svg, // don't animate in old IE
                marginRight: 20,
                events: {
                    load: function () {
               

                        // set up the updating of the chart each second
                        var series = this.series[0];
                        var series2 = this.series[1];
                        setInterval(function () { 
                            series.addPoint([getVariableX(),getVariableY()], true, true);
                            series2.addPoint([getVariableX(),getVariableZ()], true, true);
                        }, 500);
                    }
                }
            },
            
            credits: {
                enabled: false
            },
            
            title: {
                text: 'Live random data'
            },
            xAxis: {
                type: 'integer',
                tickPixelInterval: 150,		
            },
            yAxis: {
                title: {
                    text: 'Value'
                },
				
                plotLines: [{
                    value: 0,
                    width: 1,
                    color: '#808080'
                }]
            },
            tooltip: {
                formatter: function () {
                    return '<b>' + this.series.name + '</b><br/>' +
                        'X: '+Highcharts.numberFormat(this.x,2) + '<br/>' +
                        'Y: '+Highcharts.numberFormat(this.y, 2);
                }
            },
            legend: {
                enabled: false
            },
            exporting: {
                enabled: false
            },
			
            series: [{
			    	   name: 'Random data',
			    	   data: initialData
            },{
               data: anormDetec
            }]
        });
    });
    });
});