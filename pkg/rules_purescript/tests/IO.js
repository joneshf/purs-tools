"use strict";

exports.discard = function (io) {
    return function (callback) {
        return function () {
            io();
            return callback({})();
        };
    };
};

exports.println = function (message) {
    return function () {
        console.log(message);
        return {};
    };
};
