const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  mode: process.env.NODE_ENV || 'development',
  entry: './src/main.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'www'),
    clean: true,
  },
  devtool: 'source-map',
  devServer: {
    port: 3000,
    static: path.join(__dirname, 'www'),
    hot: true,
  },
  plugins: [
    new CopyWebpackPlugin({
      patterns: [
        { from: 'src-web/index.html', to: 'index.html' },
      ],
    }),
  ],
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { 
            loader: 'elm-reloader',
          },
          {
            loader: 'elm-webpack-loader',
            options: {
              cwd: __dirname,
              debug: true
            }
          }
        ]
      }
    ]
  }
};