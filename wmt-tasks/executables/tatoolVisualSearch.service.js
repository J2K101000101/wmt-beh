
tatool.factory('tatoolVisualSearch', ['executableUtils', 'stimulusServiceFactory', 'inputServiceFactory', 'dbUtils', 'timerUtils',
  function (executableUtils, stimulusServiceFactory, inputServiceFactory, dbUtils, timerUtils) {

    var visualSearch = executableUtils.createExecutable();
    //default elements:fixation time and set size
    var FIXATION_CROSS_DURATION_DEFAULT = 1000;
    var ENCODING_DURATION_DEFAULT = 200;
    var BLANK_INTERVAL_DURATION_DEFAULT = 1000;
    var DISPLAY_DURATION_DEFAULT = 5000;
    var SET_SIZES = [8, 16, 24];
    var N_TRIALS = 20;


    visualSearch.prototype.init = function () {
      var promise = executableUtils.createPromise();


      this.counter = 0;
      this.stimulusService = stimulusServiceFactory.createService(this.stimuliPath);
      this.inputService = inputServiceFactory.createService(this.stimuliPath);
      //this fixation time and timer; setsize
      this.encodingDuration = (this.encodingDuration) ? this.encodingDuration : ENCODING_DURATION_DEFAULT;
      this.fixationDuration = (this.fixationDuration) ? this.fixationDuration : FIXATION_CROSS_DURATION_DEFAULT;
      this.blankDuration = (this.blankDuration) ? this.blankDuration : BLANK_INTERVAL_DURATION_DEFAULT;
      this.displayDuration = (this.displayDuration) ? this.displayDuration : DISPLAY_DURATION_DEFAULT;
      this.encodingTimer = timerUtils.createTimer(this.encodingDuration, true, this);
      this.fixationTimer = timerUtils.createTimer(this.fixationDuration, true, this);
      this.displayTimer = timerUtils.createTimer(this.displayDuration, true, this);
      this.blankTimer = timerUtils.createTimer(this.blankDuration, true, this);
      this.setSizes = (this.setSizes) ? this.setSizes.propertyValue.map(Number) : SET_SIZES;
      this.nTrials = (this.nTrials) ? this.nTrials : N_TRIALS;
      // a trialList for randomised set size for each trial in one block
      //detectionList: half will be matched; half will be changed; randomised distribution

      this.trialList = [];
      this.detectionList = [];
      //this.orientList=[];

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



      for (var i = 0; i < this.setSizes.length; i++) {
        for (var j = 0; j < this.nTrials; j++) {

          this.trialList.push(this.setSizes[i]);
          this.detectionList.push(0);

        }
      };

      //console.log('trialLIST? ', this.trialList);
      //console.log('DetectionLIST?', this.detectionList);
      

      this.trialList = executableUtils.shuffle(this.trialList);
      var halfIndex = Math.round(this.detectionList.length / 2);
      var endIndex = this.detectionList.length;
      this.detectionList.fill(0, 0, halfIndex);
      this.detectionList.fill(1, halfIndex, endIndex);
      this.detectionList = executableUtils.shuffle(this.detectionList);
      

      //console.log('trialLIST? ', this.trialList);
      //console.log('DetectionLIST?', this.detectionList);
     

      promise.resolve();

      return promise;
    };




    visualSearch.prototype.createStimulus = function () {

      //reset startTime and endTime
      this.startTime = 0;
      this.endTime = 0;

      //create a trial object
      this.trial = {};
      //this.trial.givenResponse = null;
      //this.trial.reactionTime = 0;
     // this.trial.score = null;
      

      //set setSizes in this trial
      var n = executableUtils.getNext(this.trialList, this.counter);
      this.trial.setSize = n;

      //set detection in this trial
      var sd = executableUtils.getNext(this.detectionList, this.counter);
      this.trial.detection = sd;

      //console.log('this trial setsize: ', this.trial.setSize);
      //console.log('match0/change1?', this.trial.detection);

      //select a ramdom probeItem to be the trageted different one
      var probeIndex = executableUtils.getRandomInt(1, n);
      this.trial.probeItem = probeIndex;

       //console.log('probeItem:', probeIndex)

      // creat stimuli array
      this.stimuli = [];

      var angle = 0;

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
        


        //add each stimulus to stimuli
        this.stimuli.push(stimulus);

      }
              
      //console.log('stimuli', this.stimuli)

      // increment trial index counter
      this.counter++;
      //console.log('counter:',this.counter);
    };

    visualSearch.prototype.processResponse = function (response) {
      this.trial.reactionTime = this.endTime - this.startTime;
      this.trial.givenResponse = response;

     // console.log('response',response)


      if (this.trial.detection == this.trial.givenResponse) {
        this.trial.score = 1;
        //console.log('score=1');
      }
      else { this.trial.score = 0; 
              // console.log('score=0')
            }
      
      //console.log('trial:',this.trial);

      dbUtils.saveTrial(this.trial).then(executableUtils.stop);
    };

    return visualSearch;
  }]);