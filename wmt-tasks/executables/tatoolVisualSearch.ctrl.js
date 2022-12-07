tatool.controller('tatoolVisualSearchCtrl', ['$scope', 'service', 'executableUtils', 'dbUtils',
    function ($scope, service, executableUtils, dbUtils) {

        $scope.stimulusService = service.stimulusService;
       
        $scope.inputService = service.inputService;
        

        var canvas;
        var canvasElement;

        $scope.start = function () {
            service.createStimulus();



            canvasElement = document.getElementById('canvas');

            canvasElement.width = window.innerWidth;
            canvasElement.height = window.innerHeight;

            canvas = new fabric.Canvas('canvas');
            canvas.selection = false;
            canvas.backgroundColor = 'rgb(128,128,128)';
            canvas.defaultCursor = 'none';

            var text = new fabric.Text('+', { frontSize: 60, fill: 'white' });
            canvas.add(text);
            text.center();
          
            service.fixationTimer.start(showWhich);

        };



        

        function showWhich() {
            //show match or change stimuli


            //console.log('in showWhich', service.trial.detection)


            switch (service.trial.detection) {

                //match
                case 0:
                    showMatch();
                    break;

                //change
                case 1:
                    showChange();
            }
        }



        function showMatch() {

            canvas.remove(...canvas.getObjects());

            var n = service.trial.setSize;
            var cheeses = [];
            var triangle;
            var bubble1;
            var bubble2;
            var bubble3;
            var cheeses;
            var x, y, angle;

            //create n triangles' properties (x,y) & filter out the overlapped
            for (var i = 0; i < n; i++)
            //while(cheeses.length < n)
            {

                x = executableUtils.getRandomInt(111, canvas.width - 111);
                y = executableUtils.getRandomInt(111, canvas.height - 111);


                triangle = new fabric.Triangle({
                    width: 52,
                    height: 98,
                    left: x,
                    top: y,
                    angle: 0,
                    fill: 'white'

                });

                bubble1 = new fabric.Circle({//left
                    radius: 4,
                    fill: 'rgb(128,128,128)',
                    left: x + 9,//x+triangle.width/4-bubble1.radius
                    top: y + 45//y+triangle.height/2-bubble1.radius

                });
                bubble2 = new fabric.Circle({//bottom
                    radius: 4,
                    fill: 'rgb(128,128,128)',
                    left: x + 22, //x+(triangle.width/2)-bubble2.radius
                    top: y + 94 //y+triangle.height-bubble2.radius
                });
                bubble3 = new fabric.Circle({
                    radius: 4,
                    fill: 'rgb(128,128,128)',
                    left: x + 35, //x+triangle.width*0.75-bubble1.radius
                    top: y + 45 //y+triangle.height/2-bubble1.radius
                });

                var cheese = new fabric.Group([triangle, bubble1, bubble2, bubble3], {
                    originX: 'center',
                    originY: 'center',
                    selectable: false


                });


                cheeses.push(cheese);
            };

            //console.log('cheeses in showMatch:',cheeses);

            var c = Math.ceil(Math.sqrt(n));
            //console.log("c:", c)
            var x0 = 0;
            var y0 = 0;
            var x, y;
            var X = [];
            var Y = [];
            var increX = canvas.width / (c + 1) + 10;
            var increY = canvas.height / (c + 1) + 10;

            for (m = 1; m <= c; m++) {
                for (n = 1; n <= c; n++) {
                    x = x0 + increX * m;
                    y = y0 + increY * n;
                    X.push(x);
                    Y.push(y);
                }
            }
            // console.log('X list:', X);
            //console.log('Y list', Y);

            for (i = 0; i < cheeses.length; i++) {
                cheeses[i].left = X[i];
                cheeses[i].top = Y[i];
                cheeses[i].angle = service.stimuli[i].angle;
                canvas.add(cheeses[i])
            };
            service.startTime = executableUtils.getTiming();
            //console.log('startTime in showMatch',service.startTime);
          

            //console.log('detection cheeses in showMatch', cheeses);


            service.inputService.enable();

            service.displayTimer.start(timerUp);
                
                
            if (service.showKeys.propertyValue === true) {
                service.inputService.show();
                
            }
            
                
            


        }


        function showChange() {

            canvas.remove(...canvas.getObjects());
            var cheeses = [];
            var triangle;
            var bubble1;
            var bubble2;
            var bubble3;
            var cheeses;
            var x, y, angle;
            var n = service.trial.setSize


            var j = service.trial.probeItem - 1;
            for (var i = 0; i < n; i++) {

                x = executableUtils.getRandomInt(111, canvas.width - 111);
                y = executableUtils.getRandomInt(111, canvas.height - 111);
                if (i !== j) {


                    triangle = new fabric.Triangle({
                        width: 52,
                        height: 98,
                        left: x,
                        top: y,
                        angle: 0,
                        fill: 'white'

                    });

                    bubble1 = new fabric.Circle({//left
                        radius: 4,
                        fill: 'rgb(128,128,128)',
                        left: x + 9,//x+triangle.width/4-bubble1.radius
                        top: y + 45//y+triangle.height/2-bubble1.radius

                    });
                    bubble2 = new fabric.Circle({//bottom
                        radius: 4,
                        fill: 'rgb(128,128,128)',
                        left: x + 22, //x+(triangle.width/2)-bubble2.radius
                        top: y + 94 //y+triangle.height-bubble2.radius
                    });
                    bubble3 = new fabric.Circle({
                        radius: 4,
                        fill: 'rgb(128,128,128)',
                        left: x + 35, //x+triangle.width*0.75-bubble1.radius
                        top: y + 45 //y+triangle.height/2-bubble1.radius
                    });

                    var cheese = new fabric.Group([triangle, bubble1, bubble2, bubble3], {
                        originX: 'center',
                        originY: 'center',
                        selectable: false


                    });


                    cheeses.push(cheese);
                   // console.log('non-probed cheeses in showChange:', cheeses);


                } else {
                    //console.log('insert changed one at: ', j + 1)
                    var missingBublle = executableUtils.getRandomInt(1, 3);
                     service.trial.probeBubble=missingBublle;

                    switch (missingBublle) {
                        case 1:
                            triangle = new fabric.Triangle({
                                width: 52,
                                height: 98,
                                left: x,
                                top: y,
                                angle: 0,
                                fill: 'white'

                            });


                            bubble2 = new fabric.Circle({//bottom
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 22, //x+(triangle.width/2)-bubble2.radius
                                top: y + 94 //y+triangle.height-bubble2.radius
                            });
                            bubble3 = new fabric.Circle({
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 35, //x+triangle.width*0.75-bubble1.radius
                                top: y + 45 //y+triangle.height/2-bubble1.radius
                            });

                            var cheese = new fabric.Group([triangle, bubble2, bubble3], {
                                originX: 'center',
                                originY: 'center',
                                selectable: false


                            });

                            break;
                        case 2:
                            triangle = new fabric.Triangle({
                                width: 52,
                                height: 98,
                                left: x,
                                top: y,
                                angle: 0,
                                fill: 'white'

                            });

                            bubble1 = new fabric.Circle({//left
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 9,//x+triangle.width/4-bubble1.radius
                                top: y + 45//y+triangle.height/2-bubble1.radius

                            });

                            bubble3 = new fabric.Circle({
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 35, //x+triangle.width*0.75-bubble1.radius
                                top: y + 45 //y+triangle.height/2-bubble1.radius
                            });

                            var cheese = new fabric.Group([triangle, bubble1, bubble3], {
                                originX: 'center',
                                originY: 'center',


                            });

                            break;
                        case 3:

                            triangle = new fabric.Triangle({
                                width: 52,
                                height: 98,
                                left: x,
                                top: y,
                                angle: 0,
                                fill: 'white'

                            });

                            bubble1 = new fabric.Circle({//left
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 9,//x+triangle.width/4-bubble1.radius
                                top: y + 45//y+triangle.height/2-bubble1.radius

                            });
                            bubble2 = new fabric.Circle({//bottom
                                radius: 4,
                                fill: 'rgb(128,128,128)',
                                left: x + 22, //x+(triangle.width/2)-bubble2.radius
                                top: y + 94 //y+triangle.height-bubble2.radius
                            });


                            var cheese = new fabric.Group([triangle, bubble1, bubble2], {
                                originX: 'center',
                                originY: 'center',
                                selectable: false


                            });

                    }
                    cheeses.push(cheese);
                    //console.log('inserted cheeses with a missing bubble',cheese);


                    //console.log('which bubble is missing?',missingBublle);
                   
                    //console.log('all cheeses with changed:',cheeses);

                }
            };



            var c = Math.ceil(Math.sqrt(n));

            var x0 = 0;
            var y0 = 0;
            var x, y;
            var X = [];
            var Y = [];
            var increX = canvas.width / (c + 1) + 10;
            var increY = canvas.height / (c + 1) + 10;

            for (m = 1; m <= c; m++) {
                for (n = 1; n <= c; n++) {
                    x = x0 + increX * m;
                    y = y0 + increY * n;
                    X.push(x);
                    Y.push(y);
                }
            }
            //console.log('X list:', X);
            //console.log('Y list', Y);

            for (i = 0; i < cheeses.length; i++) {
                cheeses[i].left = X[i];
                cheeses[i].top = Y[i];
                cheeses[i].angle = service.stimuli[i].angle;
                canvas.add(cheeses[i])
            };
            service.startTime = executableUtils.getTiming();
            //console.log('startTime in showChange',service.startTime)

            //console.log('detection cheeses', cheeses);
           // console.log('showChange');

          service.inputService.enable(); 
          
          service.displayTimer.start(timerUp);
            
                
            if (service.showKeys.propertyValue === true) {
                service.inputService.show();
                
           
            }
            //service.startTime = service.stimulusService.show();



        }

        function timerUp() {
            //console.log('timerUp');
            service.inputService.disable();
           // service.endTime = service.stimulusService.hide();
            service.endTime  = executableUtils.getTiming();
            //console.log('endTime in Timerup',service.endTime);

            service.processResponse('omission');
            
        }




 











        $scope.inputAction = function (input, timing, event) {

            
            service.inputService.disable();

            //service.stimulusService.hide();
            canvas.off();
            canvas.dispose(); 

            service.endTime = timing;
            service.endTime  = executableUtils.getTiming();
            
            service.processResponse(input.givenResponse);
            //dbUtils.saveTrial(this.trial).then(executableUtils.stop);


            //console.log('givenResponse:',service.trial.givenResponse);
        };

    

    }]);