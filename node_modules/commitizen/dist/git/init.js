"use strict";

var cov_1vuql1smi = function () {
  var path = "/home/travis/build/commitizen/cz-cli/src/git/init.js";
  var hash = "ae5fc5e3b2317c0b2f76888f1b605cc82480e6b1";
  var global = new Function("return this")();
  var gcv = "__coverage__";
  var coverageData = {
    path: "/home/travis/build/commitizen/cz-cli/src/git/init.js",
    statementMap: {
      "0": {
        start: {
          line: 9,
          column: 2
        },
        end: {
          line: 9,
          column: 61
        }
      }
    },
    fnMap: {
      "0": {
        name: "init",
        decl: {
          start: {
            line: 8,
            column: 9
          },
          end: {
            line: 8,
            column: 13
          }
        },
        loc: {
          start: {
            line: 8,
            column: 25
          },
          end: {
            line: 10,
            column: 1
          }
        },
        line: 8
      }
    },
    branchMap: {},
    s: {
      "0": 0
    },
    f: {
      "0": 0
    },
    b: {},
    _coverageSchema: "43e27e138ebf9cfc5966b082cf9a028302ed4184",
    hash: "ae5fc5e3b2317c0b2f76888f1b605cc82480e6b1"
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
exports.init = init;

var _child_process = _interopRequireDefault(require("child_process"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Synchronously creates a new git repo at a path
 */
function init(repoPath) {
  cov_1vuql1smi.f[0]++;
  cov_1vuql1smi.s[0]++;

  _child_process.default.spawnSync('git', ['init'], {
    cwd: repoPath
  });
}