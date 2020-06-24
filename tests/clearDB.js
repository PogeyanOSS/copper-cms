var MongoClient = require('mongodb').MongoClient;
const cmis = require("cmis");
var mongoURL = process.env.MONGO_CLIENT_URL;
var username = process.env.CMIS_USERNAME;
var password = process.env.CMIS_PASSWORD;
var repoName = process.env.CMIS_REPO_ID;
var url = process.env.HOST_URL + '/sapp-cms/' + repoName;

async function clearDB() {
    try {
        var session = await createSession();
        await session.resetCache();
        console.log("Cache cleared in CMIS");
        await new Promise((resolve, reject) => {
            MongoClient.connect(mongoURL, function (err, client) {
                var dbname = repoName.replace("-", "_");
                var db = client.db(dbname);
                if (err) reject(err);
                console.log("Connected to Database!");
                console.log("db object points to the database : " + db.databaseName);
                db.dropDatabase(function (err, result) {
                    console.log("Error : " + err);
                    if (err) reject(err);
                    console.log("Operation Success ? " + result);
                    client.close();
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