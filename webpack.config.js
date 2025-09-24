const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  mode: process.env.NODE_ENV || 'development',
  entry: './public/main.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'www'),
    clean: true, // Clean www folder before each build
  },
  devtool: 'source-map', // Enable source maps for debugging
  plugins: [
    new CopyWebpackPlugin({
      patterns: [
        { from: 'src-web/index.html', to: 'index.html' },
        { from: 'public/elm.js', to: 'elm.js', noErrorOnMissing: true },
      ],
    }),
  ],
};