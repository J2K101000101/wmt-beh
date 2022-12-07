//'use strict';

tatool
  .factory('tatoolOrientationDetection', ['executableUtils', 'stimulusServiceFactory', 'inputServiceFactory', 'dbUtils', 'timerUtils',
    function (executableUtils, stimulusServiceFactory, inputServiceFactory, dbUtils, timerUtils) {

      var OrientationDetection = executableUtils.createExecutable();

      var FIXATION_CROSS_DURATION_DEFAULT = 1000;
      var ENCODING_DURATION_DEFAULT = 200;
      var BLANK_INTERVAL_DURATION_DEFAULT = 1000;
      var SET_SIZES = [4];
      var N_TRIALS = 60;

      OrientationDetection.prototype.init = function () {


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
            this.detectionList.push(0);
          }
        }

        this.trialList = executableUtils.shuffle(this.trialList);

        var halfIndex = Math.round(this.detectionList.length / 2);
        var endIndex = this.detectionList.length;
        this.detectionList.fill(0, 0, halfIndex);
        this.detectionList.fill(1, halfIndex, endIndex);
        this.detectionList = executableUtils.shuffle(this.detectionList);

        //console.log('trialLIST? ',this.trialList);
        //console.log('DetectionLIST?',this.detectionList);



        promise.resolve();

        return promise;
      };

      //create stimuli and set properties
      OrientationDetection.prototype.createStimulus = function () {
        //reset startTime & endTime
        this.startTime = 0;
        this.endTime = 0;

        // a trial object

        this.trial = {};
        //this.trial.givenResponse = null;
        //this.trial.reactionTime = 0;
       // this.trial.score = null;
        // set the set size
        var n = executableUtils.getNext(this.trialList, this.counter);
        this.trial.setSize = n;

        // set the detection( match0/change1)
        var sd = executableUtils.getNext(this.detectionList, this.counter);
        this.trial.detection = sd;

        //console.log('this trial setsize: ',this.trial.setSize);
        //console.log('match0/change1?',this.trial.detection)

        // creat empty maxium(constant four here) stimuli values (angles)

        //var maxStimuli = Math.max.apply(Math, this.setSizes);
        var maxStimuli = Math.max(this.setSizes)
        for (var j = 1; j <= maxStimuli; j++) {
          this.trial['stimulusValue' + j] = '';
        }




        // select random probe item
        var probeIndex = executableUtils.getRandomInt(1, n);
        this.trial.probeItem = probeIndex;

        //console.log('probeItem:',probeIndex)

        // creat stimuli array
        this.stimuli = [];

        var angle = 0;

        for (var i = 1; i <= n; i++) {
          // every stimulus object (angle, probe)
          var stimulus = {};
          //console.log('which stimulus for this trial:',i)
          //console.log(' This stimulus: ',stimulus)
          // random angle
          stimulus.angle = executableUtils.getRandomInt(0, 359);



          //probeAngle 
          stimulus.probe = (probeIndex == i) ? true : false;
          //console.log('is this a probe?',stimulus.probe)

          this.trial['stimulusValue' + i] = stimulus.angle;

          //console.log('stimulus angle',stimulus.angle )

          this.trial.probeAngle = (probeIndex == i) ? stimulus.angle : this.trial.probeAngle;
          //console.log('whats probeAngle? ', this.trial.probaAngle ) *undefied*



          //add each stimulus to stimuli
          this.stimuli.push(stimulus);
          //console.log('stimuli[i]',stimuli)

        }

        //console.log('stimuli',this.stimuli)

        // increment trial index counter
        this.counter++;

        //console.log('counter?',counter)

      };
      //console.log('trial?',trial)


      // Process given response and stop executable
      OrientationDetection.prototype.processResponse = function (response) {

        this.trial.reactionTime = this.endTime - this.startTime;
        // S=match D=change
        //console.log('endtime',this.endTime);
        //console.log('starttime',this.startTime);


        this.trial.givenResponse = response;
        if (this.trial.detection == this.trial.givenResponse) {
          this.trial.score = 1;
        } else {
          this.trial.score = 0;
        }
        dbUtils.saveTrial(this.trial).then(executableUtils.stop);
      };

      //console.log('give response?',this.trial.givenResponse);
      //console.log('corroct response?', this.trial.detection);
      // console.log('score?', this.trial.score)

      return OrientationDetection;
    }]);
