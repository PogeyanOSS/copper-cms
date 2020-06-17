const path = require('path');
const appEnv = process.env.NODE_ENV || 'development';
const isProduction = appEnv === 'production';
const exclude = /node_modules/;

module.exports = {
    entry: ['./a.js'],
    target: "node",
    devtool: "none",
    mode: isProduction ? "production" : "development",
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'clearDB.js',
        libraryTarget: 'commonjs'
    },
    resolve: {
        extensions: ['.ts', '.js'] //resolve all the modules other than index.ts
    },
    module: {
        rules: [
            {
                "test": /\.(ts|tsx)?$/,
                use:
                    [
                    ],
                "exclude": exclude,
            },
            {
                test: /\.js$/,
                use: [
                ],
                "exclude": exclude
            },
        ]
    },
}