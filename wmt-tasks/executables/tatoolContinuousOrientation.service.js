'use strict';

tatool.factory('tatoolContinuousOrientation', ['executableUtils', 'dbUtils', 'timerUtils', 'stimulusServiceFactory', 'inputServiceFactory',
  function (executableUtils, dbUtils, timerUtils, stimulusServiceFactory, inputServiceFactory) {

    var ContinuousOrientation = executableUtils.createExecutable();
    
    /** 
     * IMPORTANT: Use the prefix "tatoolContinuous" for the Executable name in the Editor to avoid flickering due to different background color.
     */

    var ENCODING_DURATION_DEFAULT = 500;
    var FIXATION_CROSS_DURATION_DEFAULT = 500;
    var BLANK_INTERVAL_DURATION_DEFAULT = 500;
    var SET_SIZES = [2, 4, 6];
    var N_TRIALS = 20;

    //  Initialze variables at the start of every session
    ContinuousOrientation.prototype.init = function () {
      var deferred = executableUtils.createPromise();

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
      // template properties
      this.stimulusService = stimulusServiceFactory.createService(this.stimuliPath);

      // set sizes
      this.setSizes = (this.setSizes) ? this.setSizes.propertyValue.map(Number) : SET_SIZES;
      this.nTrials = (this.nTrials) ? this.nTrials : N_TRIALS;


      // timing properties
      this.encodingDuration = (this.encodingDuration) ? this.encodingDuration : ENCODING_DURATION_DEFAULT;
      this.fixationDuration = (this.fixationDuration) ? this.fixationDuration : FIXATION_CROSS_DURATION_DEFAULT;
      this.blankDuration = (this.blankDuration) ? this.blankDuration : BLANK_INTERVAL_DURATION_DEFAULT;
      this.encodingTimer = timerUtils.createTimer(this.encodingDuration, true, this);
      this.fixationTimer = timerUtils.createTimer(this.fixationDuration, true, this);
      this.blankTimer = timerUtils.createTimer(this.blankDuration, true, this);

      // trial counter property
      this.counter = 0;

      // create list of set sizes to ensure even distribution
      this.trialList = [];

      for (var i = 0; i < this.setSizes.length; i++) {
        for (var j = 0; j < this.nTrials; j++) {
          this.trialList.push(this.setSizes[i]);
        }
      }

      this.trialList = executableUtils.shuffle(this.trialList);

      //console.log(this.trialList);

      // prepare stimuli
      deferred.resolve();


      return deferred;
    };

    // Create stimulus and set properties
    ContinuousOrientation.prototype.createStimulus = function () {
      // reset executable properties
      this.startTime = 0;
      this.endTime = 0;

      // create new trial
      this.trial = {};
      //use the following line if completely randomizing:
      //var n = this.setSizes[executableUtils.getRandomInt(0, this.setSizes.length - 1)];
      var n = executableUtils.getNext(this.trialList, this.counter);
      this.trial.setSize = n;

      //console.log(this.trial.setSize);

      // create empty stimulus values
      var maxStimuli = Math.max.apply(Math, this.setSizes);
      for (var j = 1; j <= maxStimuli; j++) {
        this.trial['stimulusValue' + j] = '';
      }

      // select random probe item
      var probeIndex = executableUtils.getRandomInt(1, n);
      this.trial.probeItem = probeIndex;

      this.stimuli = [];
      var angle = 0;
      for (var i = 1; i <= n; i++) {
        var stimulus = {};
        stimulus.angle = executableUtils.getRandomInt(0, 360); // random angle
        stimulus.probe = (probeIndex == i) ? true : false;
        this.trial['stimulusValue' + i] = stimulus.angle;
        this.trial.probeAngle = (probeIndex == i) ? stimulus.angle : this.trial.probeAngle;
        this.stimuli.push(stimulus);
      }

      // increment trial index counter
      this.counter++;
    };
    
    // Process given response and stop executable
    ContinuousOrientation.prototype.processResponse = function (givenResponse, canvas) {
      this.trial.reactionTime = this.endTime - this.startTime;

      canvas.dispose();

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
      this.trial.deviationAbs = Math.abs(this.trial.deviation);

     
      dbUtils.saveTrial(this.trial).then(executableUtils.stop);
  
    
    };

    return ContinuousOrientation;

  }]);
