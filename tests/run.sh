export MONGO_CLIENT_URL=$1
export CMIS_USERNAME=$2
export CMIS_PASSWORD=$3
export CMIS_REPO_ID=$4
export HOSTURL=$5
echo "web packing cleardb js script"

npm run build
echo "doing to git directory"

cd dist/

echo "clearing the db"

node clearDB.js

echo "runing tcks tests"

cd ../../

mvn install -pl chemistry-opencmis-test-tck

echo "clearing the db"

cd test/dist/

node clearDB.js