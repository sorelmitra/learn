const nodeExternals = require('webpack-node-externals');
const path = require('path');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const NodemonPlugin = require('nodemon-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const Dotenv = require('dotenv-webpack');

module.exports = {
  entry: `${__dirname}/tools/mockServer/src/index.ts`,
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'babel-loader'
      }
    ]
  },
  externals: [nodeExternals()],
  resolve: {
    extensions: ['.ts', '.js']
  },
  output: {
    filename: 'mockServer.js',
    path: path.resolve(__dirname, 'tools/mockServer/bundle')
  },
  target: 'node',
  mode: 'development',
  watch: true,
  watchOptions: {
    aggregateTimeout: 300,
    poll: 1000
  },
  devtool: 'inline-source-map',
  plugins: [
    new CleanWebpackPlugin(),
    new ForkTsCheckerWebpackPlugin(),
    new NodemonPlugin(),
    new CopyWebpackPlugin({
      patterns: [ { from: `${__dirname}/tools/mockServer/static` } ]
    })
  ]
};
