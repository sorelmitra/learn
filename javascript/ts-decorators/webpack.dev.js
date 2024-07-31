const { merge } = require('webpack-merge');
const common = require('./webpack.common.js');
const NodemonPlugin = require('nodemon-webpack-plugin');
const Dotenv = require('dotenv-webpack');
const path = require('path');
const webpack = require('webpack');

const env = process.env.ENV;

module.exports = merge(
  {
    mode: 'development',
    watch: true,
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
    devtool: 'inline-source-map',
    plugins: [
      new NodemonPlugin(),
      new webpack.DefinePlugin({
        'process.env.NODE_ENV': JSON.stringify('development')
      }),
      new Dotenv({
        path: path.resolve(__dirname, `.${env}.env`),
        systemvars: true,
        safe: false
      })
    ]
  },
  common
);
