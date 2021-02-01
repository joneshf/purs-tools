"use strict";

const child_process = require("child_process");
const process = require("process");

exports.argv = function () {
    return process.argv.slice(2);
};

exports.bind = function (io) {
    return function (callback) {
        return function () {
            const value = io();
            return callback(value)();
        };
    };
};

exports.discard = function (io) {
    return function (callback) {
        return function () {
            io();
            return callback({})();
        };
    };
};

exports.exec = function (path) {
    return function () {
        const result = child_process.execSync(path);
        return result.toString();
    };
};

exports.exit = function (code) {
    return function () {
        process.exit(code);
    };
};

exports.forEach = function (values) {
    return function (callback) {
        return function () {
            values.forEach(function (value) {
                callback(value)();
            });
            return {};
        };
    };
};

exports.println = function (message) {
    return function () {
        console.log(message);
        return {};
    };
};
