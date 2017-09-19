const express = require('express');
const app = express();

const bodyParser = require('body-parser');
app.use(bodyParser.json());

const mongoClient = require('mongodb').MongoClient;
const mongodbURL = "mongodb://127.0.0.1:27017/assignmentsdb";

const path = require('path');

function assignmentsManager(task, data, okCallBack) {
  const assignmentCollection = "assignments";
  mongoClient.connect(mongodbURL, (err, db) => {
    if (err) {
      console.log(err);
      res.status(500).send({error: 'Problems connecting to DB'});
    }
    const coll = db.collection(assignmentCollection);

    if (task === "fetchAll") {
      var returnValue = {};
      coll.find({}).toArray((err, result) => {
        if (err) {
          console.log("Error converting json to array");
          db.close();
          return null;
        } else {
          db.close();
          okCallBack(result);
        }
      });
      db.close();
      return {};
    }

    else if (task === "insert") {
      coll.insert(data.assignment);
    }

    else if (task === "update") {
      updateObject = {$set : {'done' : data.status}};
      coll.update(data.assignment, updateObject);
    }

    else if (task === "delete") {
      coll.remove(data.assignment);
    }

    else {
      console.log("Unknown command");
    }

    db.close();
    okCallBack();
  });
}


app.get('/assignments', (req, res) => {
  assignmentsManager("fetchAll", null, (result) => {
    res.json(result);
  });
});

app.post('/deleteAssignment', (req, res) => {
  if (!req.body || !req.body.title || !req.body.course || !req.body.due) {
    res.status(400).send();
    return;
  }
  assignmentsManager("delete", {assignment : req.body}, () => {
    res.status(200).send();
  });
});

app.post('/changeAssignmentStatus', (req, res) => {
  if (!req.body || !req.body.title || !req.body.course || !req.body.due) {
    res.status(400).send();
    return;
  }
  const done = req.body.done;
  delete req.body.done;
  assignmentsManager("update", {assignment : req.body, status : done}, () => {
    res.status(200).send();
  });
});

app.post('/addAssignment', (req, res) => {
  if (!req.body || !req.body.title || !req.body.course || !req.body.due) {
    res.status(400).send();
    return;
  }
  assignmentsManager("insert", {assignment : req.body}, () => {
    res.status(201).send();
  });
});

app.use('/', express.static('public'));

app.listen(3000, function () {
  console.log('Server for Submit System is now listening at port 3000');
});
