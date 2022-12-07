//'use strict';

tatool
  .factory('tatoolShapeReproduction', ['executableUtils', 'stimulusServiceFactory', 'inputServiceFactory', 'dbUtils', 'timerUtils',
    function (executableUtils, stimulusServiceFactory, inputServiceFactory, dbUtils, timerUtils) {

      var ShapeReproduction = executableUtils.createExecutable();

      var FIXATION_CROSS_DURATION_DEFAULT = 1000;
      var ENCODING_DURATION_DEFAULT = 200;
      var BLANK_INTERVAL_DURATION_DEFAULT = 1000;
      var SET_SIZES = [4];
      var N_TRIALS = 60;

      ShapeReproduction.prototype.init = function () {


        //make sure variables has been loaded
        var promise = executableUtils.createPromise();
        //showkeys? Timer?

        if (!this.showKeys) {
          this.showKeys = { propertyValue: true };
        } else {
          this.showKeys.propertyValue = (this.showKeys.propertyValue === true) ? true : false;
        }

        if (!this.timerEnabled) {
          this.timerEnabled = { propertyValue: false };
        } else {
          this.timerEnabled.propertyValue = (this.timerEnabled.propertyValue === true) ? true : false;
        }

        this.inputService = inputServiceFactory.createService(this.stimuliPath);

        this.stimulusService = stimulusServiceFactory.createService(this.stimuliPath);

        //set size
        this.setSizes = (this.setSizes) ? this.setSizes.propertyValue.map(Number) : SET_SIZES;

        //console.log('setSizes length here? ',setSizes.length);

        // the number of trials for each block
        this.nTrials = (this.nTrials) ? this.nTrials : N_TRIALS;

        // durations and timer
        this.encodingDuration = (this.encodingDuration) ? this.encodingDuration : ENCODING_DURATION_DEFAULT;
        this.fixationDuration = (this.fixationDuration) ? this.fixationDuration : FIXATION_CROSS_DURATION_DEFAULT;
        this.blankDuration = (this.blankDuration) ? this.blankDuration : BLANK_INTERVAL_DURATION_DEFAULT;
        this.encodingTimer = timerUtils.createTimer(this.encodingDuration, true, this);
        this.fixationTimer = timerUtils.createTimer(this.fixationDuration, true, this);
        this.blankTimer = timerUtils.createTimer(this.blankDuration, true, this);

        // tiral counter
        this.counter = 0;

        // a trialList for randomised set size for each trial in one block
        //detectionList: half will be matched; half will be changed; randomised distribution

        this.trialList = [];
        this.detectionList = [];

        //console.log('setSizes length here? ',setSizes.length)

        for (var i = 0; i < this.setSizes.length; i++) {
          for (var j = 0; j < this.nTrials; j++) {
            this.trialList.push(this.setSizes[i]);

          }
        }

        this.trialList = executableUtils.shuffle(this.trialList);

        //console.log('trialLIST? ', this.trialList);




        promise.resolve();

        return promise;
      };

      //create stimuli and set properties
      ShapeReproduction.prototype.createStimulus = function () {
        //reset startTime & endTime
        this.startTime = 0;
        this.endTime = 0;

        // a trial object

        this.trial = {};
        //this.trial.givenResponse = null;
        //this.trial.reactionTime = 0;
        //this.trial.score = null;
        // set the set size
        var n = executableUtils.getNext(this.trialList, this.counter);
        this.trial.setSize = n;


        //console.log('this trial setsize: ', this.trial.setSize);


        // creat empty maxium(constant four here) stimuli values (angles)

        //var maxStimuli = Math.max.apply(Math, this.setSizes);
        var maxStimuli = Math.max(this.setSizes)
        for (var j = 1; j <= maxStimuli; j++) {
          this.trial['stimulusValue' + j] = '';
        }




        // select random probe item
        var probeIndex = executableUtils.getRandomInt(1, n);
        this.trial.probeItem = probeIndex;

        //console.log('probeItem:', probeIndex)

        // creat stimuli array
        this.stimuli = [];


        for (var i = 1; i <= n; i++) {
          // every stimulus object (angle, probe)
          var stimulus = {};
          //console.log('which stimulus for this trial:', i)
          //console.log(' This stimulus: ', stimulus)
          // random angle
          stimulus.angle = executableUtils.getRandomInt(0, 359);



          //probeAngle 
          stimulus.probe = (probeIndex == i) ? true : false;
          //console.log('is this a probe?', stimulus.probe)

          this.trial['stimulusValue' + i] = stimulus.angle;

          //console.log('stimulus angle',stimulus.angle )

          this.trial.probeAngle = (probeIndex == i) ? stimulus.angle : this.trial.probeAngle;




          //add each stimulus to stimuli
          this.stimuli.push(stimulus);
          //console.log('stimuli[i]',stimuli)

        }

        //console.log('stimuli', this.stimuli)

        // increment trial index counter
        this.counter++;

        //console.log('counter?',counter)

      };
      //console.log('trial?',trial)


      // Process given response and stop executable
      ShapeReproduction.prototype.processResponse = function (givenResponse) {

        this.trial.reactionTime = this.endTime - this.startTime;
        
        // get angle within 0-360
        var currentAngle = null;
        if (givenResponse < 0) {
          currentAngle = 360 + givenResponse;
        } else if (givenResponse > 360) {
          currentAngle = givenResponse - 360;
        } else {
          currentAngle = givenResponse;
        }

        this.trial.givenResponse = currentAngle;

        var delta = givenResponse - this.trial.probeAngle;
        // deviation positive, to the right;  deviation negative, to the left
        if (delta < -180){
          this.trial.deviation = delta + 360; 
        } else if (delta > 180){
          this.trial.deviation = delta - 360;
        }else{
          this.trial.deviation = delta;
        }
        //console.log('delta',this.trial.deviation)
        this.trial.deviationAbs = Math.abs(this.trial.deviation);

        dbUtils.saveTrial(this.trial).then(executableUtils.stop);
      }

      return ShapeReproduction;
    }]);
