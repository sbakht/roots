var express = require('express');
var fs = require('fs');
var request = require('request');
var cheerio = require('cheerio');
var app = express();
let baseUrl = 'http://corpus.quran.com/qurandictionary.jsp?q=';
let count = 0;
let captureRoots;

app.get('/scrape', function(req, res) {
    let pageRoots = req.param("roots");
    fs.readFile('output.json', 'utf8', function(err, input) {
        let json = JSON.parse(input);
        let rootList;
        request(baseUrl + pageRoots, function(error, response, html) {
            console.log('Fetching base ' + baseUrl + pageRoots);
            if (!error) {
                var $ = cheerio.load(html);
                addRootData($, json, pageRoots);
                rootList = getRootList($);
                captureRoots = rootList.filter((root) => {
                    return !json[root];
                });
                captureRoots.map((root, i) => requestRoot(json, root, i));
            }
            writeToFile(json);
        });
        res.send('Check your console!')
    });
})
app.listen('3000');

function writeToFile(json, text) {
    fs.writeFile('output.json', JSON.stringify(json, null, 4), function(err) {
        if (text) {
            console.log(text);
        }
    })
}

function getRootList($) {
    let list = []
    $('#entryList option').each(function() {
        let data = $(this);
        list.push(decodeURI(data.val()));
    })
    return list;
}

function requestRoot(json, roots, i) {
    let url = baseUrl + roots;
    if (!json[roots]) {
        setTimeout(() => {
            request(url + roots, function(error, response, html) {
                console.log('Fetching ' + url);
                if (!error) {
                    var $ = cheerio.load(html);
                    addRootData($, json, roots);
                }
                writeToFile(json, "Writing " + roots);
                if (++count === captureRoots.length) {
                    console.log('All requests done!', roots);
                }
            });
        }, 3000 * (i + 1))
    } else {
        console.log('Already have ' + roots);
    }
}

function addRootData($, json, roots) {
    if (!json[roots]) {
        json[roots] = [];
        $('.l').each(function() {
            var data = $(this);
            json[roots].push(data.text());
        });
    } else {
        console.log('Already have ' + roots);
    }
}

function removeArrItem(arr, item) {
    let index = arr.indexOf(item);
    if (index > -1) {
        arr.splice(index, 1);
    }
}
exports = module.exports = app;
