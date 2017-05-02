module.exports = function (config) {
  var root = 'run/compiled/karma/test'; // same as :output-dir
  var junitOutputDir = (process.env.CIRCLE_TEST_REPORTS + '/junit') || (root + '/junit')

  config.set({
    frameworks: ['cljs-test'],
    browsers: ['Chrome', 'PhantomJS'],
    files: [
      root + '/../test.js', // same as :output-to
      {pattern: root + '/../test.js.map', included: false, watched: false},
      {pattern: root + '/**/*.+(cljs|cljc|clj|js|js.map)', included: false, watched: false}
    ],

    client: {
      args: ['oi_lang.test_runner.run_karma']
    },

    autoWatchBatchDelay: 500,

    // the default configuration
    junitReporter: {
      outputDir: junitOutputDir + '/karma', // results will be saved as $outputDir/$browserName.xml
      outputFile: undefined, // if included, results will be saved as $outputDir/$browserName/$outputFile
      suite: '' // suite will become the package name attribute in xml testsuite element
    }
  })
};
