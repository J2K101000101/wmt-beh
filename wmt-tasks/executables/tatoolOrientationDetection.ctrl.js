//'use strict';

tatool
  .controller('tatoolOrientationDetectionCtrl', ['$scope', 'service', 'executableUtils',
    function ($scope, service, executableUtils) {
      $scope.inputService = service.inputService;
      $scope.stimulusService = service.stimulusService;

      var canvas;
      var canvasElement;
      // Start execution
      $scope.start = function () {

        service.createStimulus();

        // prepare the canvas
        canvasElement = document.getElementById('canvas');
        //var ctx = canvas.getContext('2d');


        canvasElement.width = document.body.getBoundingClientRect().width;
        canvasElement.height = document.body.getBoundingClientRect().height;


        canvas = new fabric.Canvas('canvas');
        canvas.selection = false;
        canvas.backgroundColor = 'rgb(128,128,128)';
        canvas.defaultCursor = 'none';

        // logic to resize (not implemented yet)
        function reportWindowSize() {
          //canvas.setWidth( document.body.getBoundingClientRect().width );
          //canvas.setHeight( document.body.getBoundingClientRect().height );
          //canvas.calcOffset();
          //canvas.renderAll();
        }
        window.onresize = reportWindowSize;

        //cross fixation
        var text = new fabric.Text('+', { fontSize: 60, fill: 'white' });
        canvas.add(text);
        text.center();
        //text.setCoords();
        service.fixationTimer.start(showEncoding);

        // service.inputService.enable();
        // service.inputService.show();


        //service.stimulusService.show();
      };

      function showEncoding() {

        canvas.remove(...canvas.getObjects());
        // display stimuli
        var radius = 200;
        var n = service.trial.setSize;
        var x, y, angle;
        var canvasObject;

        // (x + r cos(2kπ/n), y + r sin(2kπ/n))
        // where n is the number of elements, and k is the "number" of the element you're currently positioning (between 1 and n inclusive)
        for (var i = 0; i < n; i++) {
          x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI) / n));
          y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI) / n));
          angle = service.stimuli[i].angle;

          canvasObject = new fabric.Triangle({
            width: 53//35
            , height: 98//60
            , fill: 'white'
            , left: x
            , top: y
            , selectable: false
            , angle: angle
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: true
          });
          canvas.add(canvasObject);
        }

        service.encodingTimer.start(timerUp);
        //console.log('showEncoding')        
      }
      // Show Change
      function showChange() {

        canvas.remove(...canvas.getObjects());//keep all stimuli in this case
        //canvas.defaultCursor = 'pointer';

        // move the probed triangle




        // display probe
        var radius = 200;
        var n = service.trial.setSize;
        var x, y, X, Y;
        var canvasObject;

        var angle;


        //validAngles for changed probe angle
        var validAngles = [];
        for (var i = 0; i < 360; i++) {
          validAngles.push(i);
        }
        //console.log('allAngles:', validAngles);
        var dA = service.trial.probeItem - 1;
        var deleteAngle = service.stimuli[dA].angle;
        //console.log('deleted Angle:',deleteAngle);
        var index = validAngles.indexOf(deleteAngle);
        //console.log('deleted Angle Index:',index);

        if (index > -1) {
          validAngles.splice(index, 1);
        }
        //console.log('Valid Angles:', validAngles);




        var j = service.trial.probeItem - 1;


        for (var i = 0; i < n; i++) {
          if (i !== j) {
            x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI) / n));
            y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI) / n));

            //console.log('x:',x)
            //console.log('y:',y)

            angle = service.stimuli[i].angle;




            canvasObject = new fabric.Triangle({
              width: 53//35
              , height: 98//60
              , fill: 'white'//'red'
              , left: x
              , top: y
              , selectable: false
              , angle: angle
              , hoverCursor: 'none'
              , originX: 'center'
              , originY: 'center'
              , centeredRotation: true
            });
            canvas.add(canvasObject);
            //console.log('test change stayed stimuli?',angle) 
          }

        }


        //console.log('insert at :',j+1)


        X = Math.round(canvas.width / 2 + radius * Math.cos((2 * j * Math.PI) / n));
        Y = Math.round(canvas.height / 2 + radius * Math.sin((2 * j * Math.PI) / n));

        //console.log('X:',X)
        //console.log('Y:',Y)

        var probeAngle = executableUtils.getRandom(validAngles);
        service.trial.probeAngle=probeAngle;

        var probeTriangle = new fabric.Triangle({
          width: 53//35
          , height: 98//60
          , fill: 'white'//'green'
          , left: X
          , top: Y
          , selectable: false
          , angle: probeAngle
          , hoverCursor: 'none'
          , originX: 'center'
          , originY: 'center'
          , centeredRotation: true
        });


        canvas.add(probeTriangle);

        service.startTime = executableUtils.getTiming();

        //console.log('start: ',service.startTime)

        //console.log('showChange')
        //console.log('test change probe stimuli?',probeAngle)     

        service.inputService.enable();
        service.inputService.show();


      }


      function showMatch() {

        canvas.remove(...canvas.getObjects());
        // display stimuli
        var radius = 200;
        var n = service.trial.setSize;
        var x, y, angle;
        var canvasObject;


        // (x + r cos(2kπ/n), y + r sin(2kπ/n))
        // where n is the number of elements, and k is the "number" of the element you're currently positioning (between 1 and n inclusive)
        for (var i = 0; i < n; i++) {
          x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI) / n));
          y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI) / n));
          angle = service.stimuli[i].angle;

          canvasObject = new fabric.Triangle({
            width: 53//35
            , height: 98//60
            , fill: 'white'
            , left: x
            , top: y
            , selectable: false
            , angle: angle
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: true
          });

          canvas.add(canvasObject);
          //console.log('showMatch') 

        }
        service.startTime = executableUtils.getTiming();

        //console.log('start: ',service.startTime)
        service.inputService.enable();
        service.inputService.show();
      }








      // Called by timer when time elapsed
      function timerUp() {
        canvas.remove(...canvas.getObjects());
        service.blankTimer.start(showWhich);
      }

      //console.log('timerUp')

      

      function showWhich() {
        //show match or change stimuli


        //console.log('in showWhich',service.trial.detection)


        switch (service.trial.detection) {

          //match
          case 0:
            showMatch();
            break;

          //change
          case 1:
            showChange();
        }



        // service.endTime = executableUtils.getTiming();
        //console.log('end: ',service.endTime)

        //service.processResponse(input.givenResponse);                              

      }

      //console.log('showWhich')                                    




      //detection input key actions in HTML

      $scope.inputAction = function (input, timing, event) {

        service.inputService.disable();
        canvas.off();
        canvas.dispose(); 
        //service.stimulusService.hide();

        service.endTime = timing;
        service.processResponse(input.givenResponse);
        //service.stopExecution();

        //console.log(service.trial.givenResponse);
      };



    }]);

