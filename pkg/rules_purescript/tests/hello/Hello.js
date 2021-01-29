"use strict";

exports.println = function (message) {
    return function () {
        console.log(message);
        return {};
    };
};
