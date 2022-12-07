//'use strict';

tatool
  .controller('tatoolShapeReproductionCtrl', ['$scope', 'service', 'executableUtils',
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


        canvasElement.width = window.innerWidth;
        canvasElement.height = window.innerHeight;


        canvas = new fabric.Canvas('canvas');
        canvas.selection = false;
        canvas.backgroundColor = 'rgb(128,128,128)';
        canvas.defaultCursor = 'none';

        // logic to resize (not implemented yet)
        function reportWindowSize() {
          //canvas.setWidth( window.innerWidth );
          //canvas.setHeight( window.innerHeight );
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
        var radius = 200;
        var n = service.trial.setSize;
        var x, y, angle, rad;
        var outterC, arc, innerC;

        for (var i = 0; i < n; i++) {
          x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI) / n));
          y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI) / n));
          angle = service.stimuli[i].angle;
          rad = (angle - 90) * Math.PI / 180;

          innerC = new fabric.Circle({
            radius: 30
            , fill: 'rgb(128,128,128)'
            , left: x
            , top: y
            , selectable: false
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: false
          });

          outterC = new fabric.Circle({
            radius: 60
            , fill: 'black'
            , left: x
            , top: y
            , selectable: false
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: false

          });
          arc = new fabric.Circle({
            radius: 45,
            left: x,
            top: y,
            startAngle: -90 * Math.PI / 180,
            endAngle: rad,
            stroke: 'white',
            strokeWidth: 30,
            fill: '',
            originX: 'center',
            originY: 'center',
            hoverCursor: 'none',
            selectable: false,
            centeredRotation: false
          })

          canvas.add(outterC);
          canvas.add(innerC);
          canvas.add(arc);
        }
        service.startTime = executableUtils.getTiming();
        service.encodingTimer.start(timerUp);

      }

      // Called by timer when time elapsed
      function timerUp() {
        canvas.remove(...canvas.getObjects());
        service.blankTimer.start(showProbe);
      }

      // Show probe item
      function showProbe() {

        canvas.remove(...canvas.getObjects());
        canvas.defaultCursor = 'pointer';

        // display probe
        var radius = 200;
        var n = service.trial.setSize;
        var x, y, angle, rad;
        var outterC, arc, innerC;

        // (x + r cos(2kπ/n), y + r sin(2kπ/n))
        // where n is the number of elements, and k is the "number" of the element you're currently positioning (between 1 and n inclusive)
        for (var i = 0; i < n; i++) {
          x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI) / n));
          y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI) / n));

          innerC = new fabric.Circle({
            radius: 30
            , fill: 'rgb(128,128,128)'
            , left: x
            , top: y
            , selectable: false
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: false
          });

          outterC = new fabric.Circle({
            radius: 60
            , fill: (service.trial.probeItem - 1 == i) ? 'black' : 'rgb(128,128,128)'
            , left: x
            , top: y
            , selectable: false
            , hoverCursor: 'none'
            , originX: 'center'
            , originY: 'center'
            , centeredRotation: false

          });


          arc = new fabric.Circle({
            radius: 45,
            left: x,
            top: y,
            startAngle: -90 * Math.PI / 180,
            endAngle: -89 * (Math.PI / 180),
            stroke: (service.trial.probeItem - 1 == i) ? 'white' : 'rgb(128,128,128)',
            strokeWidth: 30,
            fill: '',
            originX: 'center',
            originY: 'center',
            hoverCursor: 'none',
            selectable: false,
            centeredRotation: false
          });

          canvas.add(outterC);
          canvas.add(innerC);
          canvas.add(arc);
        }




        // mouse move and down logic
        var toCanvasPoint = function (canvas, absoluteX, absoluteY) {
          var offset = fabric.util.getElementOffset(canvas.lowerCanvasEl), localX = absoluteX - offset.left, localY = absoluteY - offset.top;
          return new fabric.Point(localX, localY);
        }, offsetToObject = function (object, point) {
          var offsetPoint = new fabric.Point(object.left, object.top);
          return point.subtract(offsetPoint);
        }, start, end;

        var probe = canvas.getObjects()[service.trial.probeItem * 3 - 1];

        // start = new fabric.Point(probe.left, probe.top-52.5);
        start = new fabric.Point(0, 0);

        // console.log('objects', probe);
        //console.log(canvas.getObjects().length);



        var oldRad = 0;

        canvas.on('mouse:down', function (opts) {
          oldRad = probe.get('endAngle') - (-89 * (Math.PI / 180));
          probe.set('endAngle', oldRad + (-89 * (Math.PI / 180)));
          //oldRad = Math.abs(probe.get('endAngle')+Math.PI);
          //console.log('oldRad ', oldRad);
        });

        canvas.on('mouse:move', function (opts) {
          //console.log('opts', opts);
          // console.log('circlePos', (probe.left), (probe.top));
          // console.log('mousePos', opts.pointer.x, opts.pointer.y);
          end = offsetToObject(probe, toCanvasPoint(canvas, opts.e.clientX, opts.e.clientY));

          /* var test1=new fabric.Circle({
                  radius:3
                , fill: 'red'
                , left: probe.left
                , top: probe.top
                , selectable: false
                , hoverCursor: 'none'
                , originX: 'center'
                , originY: 'center'
                ,centeredRotation:false
              });
  
               var test2=new fabric.Circle({
                  radius:3
                , fill: 'green'
                , left: opts.pointer.x
                , top: opts.pointer.y
                , selectable: false
                , hoverCursor: 'none'
                , originX: 'center'
                , originY: 'center'
                ,centeredRotation:false
              });
  
              canvas.add(test1);
              canvas.add(test2);*/

          var rad = Math.abs(Math.atan2((opts.pointer.x - probe.left), (opts.pointer.y - probe.top)) - Math.PI);

          //console.log('angle', rad);
          probe.set('endAngle', rad + (-89 * (Math.PI / 180)));
          start = end;
          canvas.renderAll();


          /*end = offsetToObject(probe, toCanvasPoint(canvas, opts.e.clientX, opts.e.clientY));
            var deltaRad = Math.atan2(start.x-end.x,
              start.y-end.y), oldRad = probe.get('endAngle');
            var rad = oldRad + deltaRad;
            probe.set('endAngle', rad);
            start = end;
            canvas.renderAll();
          */

        });

        canvas.on('mouse:up', function (opts) {
          service.endTime = executableUtils.getTiming();
          canvas.off();
          
          canvas.dispose(); 
          //canvas.remove(...canvas.getObjects());
          var rad = probe.get('endAngle');
          //console.log('rad', rad);

          var recordAngle = rad * 180 / Math.PI + 90;


          //console.log('Responseangle', recordAngle);
          service.blankTimer.start(function () { return service.processResponse(recordAngle); });


        });


      }

    }]);

