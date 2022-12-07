'use strict';

const { ConditionsParser } = require("survey-jquery");

/* global fabric */

tatool
  .controller('tatoolContinuousOrientationCtrl', [ '$scope', 'service', 'executableUtils',
    function ($scope, service, executableUtils) {
      console.log('test');
    var canvas;
    var canvasElement;

    // Start execution
    $scope.start = function() {
      service.createStimulus();

      // prepare the canvas
      canvasElement = document.getElementById('canvas');
      canvasElement.width  = document.body.getBoundingClientRect().width;
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

      var text = new fabric.Text('+', { fontSize: 60, fill: 'white' });
      canvas.add(text);
      text.center();
      //text.setCoords();
      service.fixationTimer.start(showEncoding);
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
        x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI)/n));
        y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI)/n));
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
      console.log('start showProbe')

      canvas.remove(...canvas.getObjects());
      canvas.defaultCursor = 'pointer';

      // display probe
      var radius = 200;
      var n = service.trial.setSize;
      var x, y;
      var canvasObject;

      // (x + r cos(2kπ/n), y + r sin(2kπ/n))
      // where n is the number of elements, and k is the "number" of the element you're currently positioning (between 1 and n inclusive)
      for (var i = 0; i < n; i++) {
        x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI)/n));
        y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI)/n));

        canvasObject = new fabric.Triangle({
          width: 53//35
          , height: 98//60
          , fill: (service.trial.probeItem-1 == i) ? 'rgb(100,100,100)' : 'rgb(128,128,128)'
          , left: x
          , top: y
          , selectable: false
          , angle: executableUtils.getRandomInt(0,360)
          , hoverCursor: 'none'
          , originX: 'center'
          , originY: 'center'
          , centeredRotation: true 
        });
        canvas.add(canvasObject);
      }

      var probe = canvas.getObjects()[service.trial.probeItem-1];
      //console.log(service.trial.probeItem-1);
      //console.log(canvas.getObjects());
      console.log(probe);
      // mouse move and down logic

      
      var toCanvasPoint = function (canvas, absoluteX, absoluteY) {
        var offset = fabric.util.getElementOffset(canvas.lowerCanvasEl), localX = absoluteX - offset.left, localY = absoluteY - offset.top;
        return new fabric.Point(localX, localY);
      }, offsetToObject = function (object, point) {
        var offsetPoint = new fabric.Point(object.left, object.top);
        return point.subtract(offsetPoint);
      }, start, end;

      start = new fabric.Point(0, 0);
      console.log('to start mouse move & down')
      canvas.on('mouse:move', canvasMouseMove);
      canvas.on('mouse:down', canvasMouseDown);

      function canvasMouseMove(opts) {
        end = offsetToObject(probe, toCanvasPoint(canvas, opts.e.clientX, opts.e.clientY));
          var deltaAngle = fabric.util.radiansToDegrees(Math.atan2(start.x,
            start.y) - Math.atan2(end.x, end.y)), oldAngle = probe.get('angle');
          var angle = oldAngle + deltaAngle;
          probe.set('angle', angle);
          start = end;
          canvas.renderAll();
          return [end.x, end.y]
      }

      function canvasMouseDown(e) {
        canvas.off();
        service.endTime = executableUtils.getTiming();
        var angle = probe.get('angle');
        var givenAngle = angle; 
        canvas.off('mouse:down', canvasMouseDown);
        canvas.off('mouse:move', canvasMouseMove);
        canvas.clear();
        service.blankTimer.start(function(){ return service.processResponse(probe.get('angle'), canvas); });
        console.log(' within mouse down angle is :', angle);
        console.log(' within mouse down givenangle is :', givenAngle);
        return givenAngle
      }
      let coord = canvasMouseMove(opt);
      let givenAngle = canvasMouseDown(e);
      console.log(' after mouse down givenangle is :', givenAngle);
      feedback(coord[0],coord[1], givenAngle);
    }
    
    //feedback
    function feedback(X,Y,givenAngle){
      canvas.remove(...canvas.getObjects());
      // display stimuli
      var radius = 200;
      var n = service.trial.setSize;
      var probeObjects;
      var givenObject;

      for (var i = 0; i < n; i++) {
        x = Math.round(canvas.width / 2 + radius * Math.cos((2 * i * Math.PI)/n));
        y = Math.round(canvas.height / 2 + radius * Math.sin((2 * i * Math.PI)/n));
        angle = service.stimuli[i].angle;

        probeObjects = new fabric.Triangle({
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
        canvas.add(probeObjects);
      }
    

        givenObject = new fabric.Triangle({
          width: 53//35
          , height: 98//60
          , fill: 'green'
          , left: X
          , top: Y
          , selectable: false
          , angle: givenAngle
          , hoverCursor: 'none'
          , originX: 'center'
          , originY: 'center'
          , centeredRotation: true 
        });
        canvas.add(givenObject);
      }
  }]);
