var gulp = require('gulp-run-seq');
var exec = require('child_process').exec;
var spawn = require('child_process').spawn;
var config = require('./config');
var Server = require('mongodb').Server;
var Db = require('mongodb').Db;
var os = require('os');
var run = require('gulp-run');
var process = require('process');
var request = require('request');
var gutil = require('gulp-util');
var child1;
var child2;
var exitCode = 0;
gulp.task('build-package', function (cb) {
    var child = exec('cd .. && mvn clean install -Dmaven.test.skip=true');
    child.stdout.on('data', function (data) {
        console.log(data);
    });
    child.stderr.on('data', function (data) {
        console.log(data);
    });
    child.on('close', function (code) {
        if (code == 1) {
            exitControl(1);
        }
        cb();
    });
});
gulp.task('validate-db', ['build-package'], function (cb) {
    validatedbs(function (err, isValid, errorMsg) {
        if (err) console.log(errorMsg)
        else if (isValid == true) console.log("database validation successful");
        else if (isValid == false) {
            console.log("database validation unsuccessfull," + errorMsg);
            exitControl(1);
        }
        cb();
    });
});
gulp.task('run-server-and-tests', ['validate-db', 'build-package'], function (cb) {
    if (os.platform() == 'win32') {
        child1 = spawn('target\\bin\\webapp.bat');
        child1.stdout.setEncoding('utf8');
        child1.stdout.on('data', function (data) {
            gutil.log(data);
        });
        child1.stderr.setEncoding('utf8');
        child1.stderr.on('data', function (data) {
            console.log(data);
        });
        child1.on('close', function (code) {
            if (code == 1) {
                exitCode = 1;
            }
            cb();
        });
        var z = 0;
        var y = 0;
        var interval = setInterval(function () {
            request(config.properties.cmis_host, function (error, response, body) {
                if (error) {
                    console.log('------------------------------waiting for proper server respons,current response is :' + error + '------------------------------');
                }
                if (!error) {
                    if (response.statusCode == 200) {
                        z++;
                        if (z == 10) {
                            clearInterval(interval);
                            child2 = spawn('runtests.bat');
                            child2.stdout.setEncoding('utf8');
                            child2.stdout.on('data', function (data) {
                                gutil.log("TEST" + data);
                            });
                            child2.stderr.setEncoding('utf8');
                            child2.stderr.on('data', function (data) {
                                console.log("TEST" + data);
                            });
                            child2.on('close', function (code) {
                                if (code == 1) {
                                    exitCode = 1;
                                }
                                var netstat = exec('FOR /F "tokens=5 delims= " %P IN (\'netstat -a -n -o ^| findstr 0.0.0.0:8089\') DO TaskKill.exe /F /PID %P');
                                netstat.stdout.setEncoding('utf8');
                                netstat.stdout.on('data', function (data) {
                                    console.log(data);
                                    gutil.log(data);
                                });
                                netstat.stderr.setEncoding('utf8');
                                netstat.stderr.on('data', function (data) {
                                    console.log(data);
                                });
                                netstat.on('close', function (code) {
                                    if (code == 1) {
                                        exitCode = 1;
                                    }
                                    cb();
                                });
                            });
                        }
                    } else {
                        y++;
                        if (y == 10) {
                            exitCode = 1;
                            console.log('------------------------------server responded with eror code :' + response.statusCode + ' please check------------------------------');
                            var netstat = exec('FOR /F "tokens=5 delims= " %P IN (\'netstat -a -n -o ^| findstr 0.0.0.0:8089\') DO TaskKill.exe /F /PID %P');
                            netstat.stdout.setEncoding('utf8');
                            netstat.stdout.on('data', function (data) {
                                console.log(data);
                                gutil.log(data);
                            });
                            netstat.stderr.setEncoding('utf8');
                            netstat.stderr.on('data', function (data) {
                                console.log(data);
                            });
                            netstat.on('close', function (code) {
                                if (code == 1) {
                                    exitCode = 1;
                                }
                                cb();
                            });
                        }
                    }
                }
            });
        }, 5000)
    } else if (os.platform() == 'linux') {
        child1 = spawn('target\\bin\\webapp.sh');
        child1.stdout.setEncoding('utf8');
        child1.stdout.on('data', function (data) {
            gutil.log("TEST" + data);
        });
        child1.stderr.setEncoding('utf8');
        child1.stderr.on('data', function (data) {
            console.log("TEST" + data);
        });
        child1.on('close', function (code) {
            if (code == 1) {
                exitCode = 1;
            }
            cb();
        });
        var x = 0;
        var w = 0;
        var s_interval = setInterval(function () {
            request(config.properties.cmis_host, function (error, response, body) {
                if (error) {
                    console.log('------------------------------waiting for proper server respons,current response is :' + error + '------------------------------');
                }
                if (!error) {
                    if (response.statusCode == 200) {
                        x++;
                        if (x == 10) {
                            clearInterval(s_interval);
                            child2 = spawn('runtests.bat');
                            child2.stdout.setEncoding('utf8');
                            child2.stdout.on('data', function (data) {
                                gutil.log("TEST" + data);
                            });
                            child2.stderr.setEncoding('utf8');
                            child2.stderr.on('data', function (data) {
                                console.log("TEST" + data);
                            });
                            child2.on('close', function (code) {
                                if (code == 1) {
                                    exitCode = 1;
                                }
                                child1.kill();
                            });
                        }
                    } else {
                        w++;
                        if (w == 10) {
                            exitCode = 1;
                            console.log('------------------------------server responded with eror code :' + response.statusCode + ' please check------------------------------');
                            child1.kill();
                        }
                    }
                }
            });
        }, 5000)
    } else {
        console.log("we currently support windows and linux only");
        exitControl(1);
    }
});
gulp.task('default', ['build-package', 'validate-db', 'run-server-and-tests'], function (cb) {
    if (exitCode == 1) {
        exitControl(exitCode);
    } else {
        exitControl(exitCode);
    }
    cb();
});
process.on('exit', function (code) {
    console.log('----------------------process exit with exitcode:' + code + '---------------------');
    process.exit(code);
});
function exitControl(ecode) {
    if (ecode == 1) {
        process.exit(1);
    }
    else if (ecode == 0) {
        process.exit(0);
    }
}
function validatedbs(callback) {
    var db = new Db(config.properties.masterdb, new Server(config.properties.hostname, config.properties.port));
    db.open(function (err, db) {
        if (err) callback(err, false, 'Couldnot connect to mongoDB,please start mongoDB at host:' + config.properties.hostname + ',port:' + config.properties.port);
        var adminDb = db.admin();
        adminDb.listDatabases(function (err, dbs) {
            if (err) callback(err);
            var dblist = new Array();
            for (var x in dbs.databases) {
                dblist.push(dbs.databases[x].name);
            }
            if (dblist.indexOf(config.properties.masterdb) >= 0) {
                if (dblist.indexOf(config.properties.contentdb) >= 0) {
                    db.close();
                    callback(null, true);
                }
                else {
                    db.close();
                    callback(null, false, config.properties.contentdb + "does not exists");
                }
            }
            else {
                db.close();
                callback(null, false, config.properties.masterdb + "does not exists");
            }
        });
    });
}