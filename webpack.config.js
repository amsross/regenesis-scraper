const path = require('path');

module.exports = {
  mode: 'development',
  entry: path.resolve(__dirname, './lib/js/src/Handler.js'),
  target: 'node',
  output: {
    filename: 'dist.js',
    path: path.resolve(__dirname, './'),
    library: {
      type: 'commonjs-module',
    }
  },
  node: {
    global: false,
    __filename: false,
    __dirname: false,
  },
  stats: {
    errorDetails: true
  }
};
