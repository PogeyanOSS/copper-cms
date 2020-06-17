var mongo = require('mongodb');
const cmis = require("cmis");
var mongoURL = process.env.MONGO_CLIENT_URL;
var username = process.env.CMIS_USERNAME;
var password = process.env.CMIS_PASSWORD;
var repoName = process.env.CMIS_REPO_ID;
var url = process.env.HOSTURL + '/sapp-cms/' + repoName;

async function clearDB() {
    try {
        var session = await createSession();
        await session.resetCache();
        console.log("Cache cleared in CMIS");
        await new Promise((resolve, reject) => {
            mongo.MongoClient.connect(mongoURL, function (err, client) {
                var dbname = repoName.replace("-", "_");
                var db = client.db(dbname);
                if (err) {
                    console.log(`Error in connecting to DB: ${dbname}, Error: ${err}`)
                    reject(err);
                }
                console.log("Connection to Database established!");
                console.log("db object points to the database: " + db.databaseName);
                db.dropDatabase(function (err, result) {
                    if (err) {
                        console.log(`Error in dropping DB: ${db.databaseName}, Error: ${err}`)
                        reject(err);
                    }
                    console.log(`Database: ${db.databaseName} dropped `);
                    client.close();
                    console.log(`Db client closed`);
                    resolve(true);
                });
            });
            resolve(true);
        });
    } catch (e) {
        console.log("Error in running clearDB script, with error: " + e);
    }
}
async function createSession() {
    const session = new cmis.CmisSession(url);
    try {
        await session.setCredentials(username, password).loadRepositories();
        const rootId = session.defaultRepository.rootFolderId;
        const repositoryName = session.defaultRepository.repositoryName;
        console.log("Connected to CMIS session with rootId:%s - repositoryName: %s", rootId, repositoryName);
        return session;
    } catch (e) {
        console.log("Error in createSession due to: ", e);
        throw e;
    }
}

clearDB();