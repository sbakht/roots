var express = require('express');
var fs = require('fs');
var request = require('request');
var cheerio = require('cheerio');
var app = express();
const cors = require('cors');
let baseUrl = 'http://corpus.quran.com/qurandictionary.jsp?q=';
let count = 0;
let captureRoots;

app.use(cors());
// to change your ports for different cors stuff:
app.set('port', process.env.PORT || 3001);
app.listen(app.get('port'), function() {
  console.log('we are listening on: ',
  app.get('port'))
});

app.get('/scrape', function(req, res) {
    let pageRoots = req.query.roots;
    console.log(pageRoots)
    pageRoots.split(',').map(roots => {
        console.log(roots);
        fs.readFile('output.json', 'utf8', function(err, input) {
            let json = JSON.parse(input || '{}');
            let rootList;
            request({
                url: baseUrl,
                qs: {q : roots}
                }, function(error, response, html) {
                console.log('Fetching base ' + baseUrl + roots);
                if (!error) {
                    var $ = cheerio.load(html);
                    // addRootData($, json, roots);
                    rootList = getRootList($);
                    captureRoots = rootList.filter((root) => {
                        return !json[root.arabic];
                    });
                    captureRoots.map((root, i) => requestRoot(json, root, i));
                }
                writeToFile(json);
            });
        });
    })
    res.send('Check your console!')
})

app.get('/surah', function(req, res) {
    const surahNum = req.query.surahNum;
    request({
        url: "http://api.alquran.cloud/surah/" + surahNum + "/en.sahih",
    }, function(error, response, json) {
        res.setHeader('Content-Type', 'application/json');
        res.send(json);
    });
});

//app.listen('3001');

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
        list.push({
            english: decodeURIComponent(data.val()),
            arabic: decodeURIComponent(data.text())
        });
    })
    return list;
}

function requestRoot(json, roots, i) {
    if (!json[roots]) {
        setTimeout(() => {
            request({
                url: baseUrl,
                qs: {q : roots.english}
            }, function(error, response, html) {
                console.log('Fetching ' + roots.english);
                if (!error) {
                    var $ = cheerio.load(html);
                    addRootData($, json, roots.arabic);
                }
                writeToFile(json, "Writing " + roots.english);
                if (++count === captureRoots.length) {
                    console.log('All requests done!', roots.english);
                }
            });
        }, 3000 * (i + 1))
    } else {
        console.log('Already have ' + roots);
    }
}

function addRootData($, json, roots) {
    if (!json[roots]) {
        const data = [];
        $('.content table').each(function() {
            let table = {name: $(this).prev().text(), data: []};
            $('tr', $(this)).each(function() {
                var $el = $(this);
                let obj = {
                    location: $(".l", $el).text(),
                    transliteration: $(".ab", $el).text(),
                    translation: $(".c2 a", $el).text(),
                    word: $(".auu", $el).text(),
                };
                table.data.push(obj);
            });
            data.push(table);
        });
        json[roots] = data;
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