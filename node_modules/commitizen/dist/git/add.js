"use strict";

var cov_v1ucos01h = function () {
  var path = "/home/travis/build/commitizen/cz-cli/src/git/add.js";
  var hash = "de42aff6417c2d8c31ffa220f836616b163f9dfc";
  var global = new Function("return this")();
  var gcv = "__coverage__";
  var coverageData = {
    path: "/home/travis/build/commitizen/cz-cli/src/git/add.js",
    statementMap: {
      "0": {
        start: {
          line: 12,
          column: 2
        },
        end: {
          line: 12,
          column: 65
        }
      },
      "1": {
        start: {
          line: 19,
          column: 2
        },
        end: {
          line: 19,
          column: 70
        }
      }
    },
    fnMap: {
      "0": {
        name: "addPath",
        decl: {
          start: {
            line: 11,
            column: 9
          },
          end: {
            line: 11,
            column: 16
          }
        },
        loc: {
          start: {
            line: 11,
            column: 28
          },
          end: {
            line: 13,
            column: 1
          }
        },
        line: 11
      },
      "1": {
        name: "addFile",
        decl: {
          start: {
            line: 18,
            column: 9
          },
          end: {
            line: 18,
            column: 16
          }
        },
        loc: {
          start: {
            line: 18,
            column: 38
          },
          end: {
            line: 20,
            column: 1
          }
        },
        line: 18
      }
    },
    branchMap: {},
    s: {
      "0": 0,
      "1": 0
    },
    f: {
      "0": 0,
      "1": 0
    },
    b: {},
    _coverageSchema: "43e27e138ebf9cfc5966b082cf9a028302ed4184",
    hash: "de42aff6417c2d8c31ffa220f836616b163f9dfc"
  };
  var coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.addPath = addPath;
exports.addFile = addFile;

var _child_process = _interopRequireDefault(require("child_process"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Synchronously adds a path to git staging
 */
function addPath(repoPath) {
  cov_v1ucos01h.f[0]++;
  cov_v1ucos01h.s[0]++;

  _child_process.default.spawnSync('git', ['add', '.'], {
    cwd: repoPath
  });
}
/**
 * Synchronously adds a file to git staging
 */


function addFile(repoPath, filename) {
  cov_v1ucos01h.f[1]++;
  cov_v1ucos01h.s[1]++;

  _child_process.default.spawnSync('git', ['add', filename], {
    cwd: repoPath
  });
}