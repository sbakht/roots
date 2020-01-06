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

function createRootToLocation() {
  fs.readFile('output.json', 'utf8', function(err, input) {
    let json = JSON.parse(input || '{}');
    let nameID = 999;
    const output = [];
    for(let root in json) {
      output.push({
        root,
        id: json[root].id,
        categories: json[root].values
      })
    }
    console.log(output);
    fs.writeFile('RootToLocation.json', JSON.stringify(output, null, 4), function(err) {
    })
  });
}

// createRootToLocation();


function createLocationToRootFromRoots() {
  fs.readFile('output.json', 'utf8', function(err, input) {
    let json = JSON.parse(input || '{}');
    let nameID = 999;
    const output = [];
    for(let root in json) {
      json[root].values.forEach(function(wordType) {
        nameID++;
        wordType.data.forEach(function(wordInfo) {
          placeWordInLocation(wordType.name, json[root].id, {...wordInfo, root}, output)
        })
      })
    }
    console.log('test');
    const bakarah = output.find(surah => surah.surahNumber == 2);
    output.sort((s1,s2) => parseInt(s1.surahNumber) < parseInt(s2.surahNumber) ? -1 : 1)
    output.map(surah => {
      surah.ayats.sort((s1,s2) => parseInt(s1.ayatNumber) < parseInt(s2.ayatNumber) ? -1 : 1)
      surah.ayats.map(ayat =>
        ayat.words.sort((s1,s2) => parseInt(s1.wordNumber) < parseInt(s2.wordNumber) ? -1 : 1)
        )
      })
    console.log(bakarah.ayats);
    console.log(bakarah.ayats.length);
    fs.writeFile('LocationToRoot.json', JSON.stringify(output, null, 4), function(err) {
    })
  });
}

createLocationToRootFromRoots();

function addRootIdToRoots() {
    fs.readFile('output.json', 'utf8', function(err, input) {
    let json = JSON.parse(input || '{}');
    let nameID = 5000;
    let wordTypeId = 3000;
    for(let root in json) {
      json[root].values.forEach(function(wordType) {
        wordType.id = wordTypeId;
        nameID++;
        wordTypeId++;
        wordType.data.forEach(function(word) {
          word.rootId = json[root].id
          word.root = root;
        }) 
      })
    }
    fs.writeFile('output.json', JSON.stringify(json, null, 4), function(err) {
    })
  });
}

// addRootIdToRoots()

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

// (1:4:2) -> {surahNum: 1, ayatNum: 4, wordNum: 2}
function locToObj(location) {
  const loc = location.substring(1, location.length -1).split(':');
  return {
    surahNumber: loc[0],
    ayatNumber: loc[1],
    wordNumber: loc[2],
  }
}


function placeWordInLocation(name, rootId, wordInfo, output) {
  const location = locToObj(wordInfo.location);
  function buildWordInfo(wordInfo) {
    return   {
              wordNumber: location.wordNumber,
              word: wordInfo.word,
              translation: wordInfo.translation,
              transliteration: wordInfo.transliteration,
              root: wordInfo.root,
              rootId,
              location: wordInfo.location
            }
  }
  const surah = output.find(surah => surah.surahNumber === location.surahNumber);
  if(!surah) {
    output.push({
      surahNumber: location.surahNumber,
      ayats: [
        {
          ayatNumber: location.ayatNumber,
          words: [buildWordInfo(wordInfo)]
        }
      ]
    })
  }else{
    const ayat = surah.ayats.find(ayat => ayat.ayatNumber === location.ayatNumber);
    if(!ayat) {
      surah.ayats.push({
          ayatNumber: location.ayatNumber,
          words: [buildWordInfo(wordInfo)]
      })
    }else{
      ayat.words.push(buildWordInfo(wordInfo))
    }
  }
}

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