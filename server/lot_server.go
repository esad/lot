package lot_server

import (
	"encoding/json"
	"fmt"
	"html/template"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"

	"github.com/gorilla/mux"

	"google.golang.org/appengine"
	"google.golang.org/appengine/datastore"
)

type Pickle struct {
	Data string
}

const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

func RandString(n int) string {
	b := make([]byte, n)
	for i := range b {
		b[i] = alphabet[rand.Intn(len(alphabet))]
	}
	return string(b)
}

func isJSON(s string) bool {
	var js map[string]interface{}
	return json.Unmarshal([]byte(s), &js) == nil
}

func init() {
	router := mux.NewRouter()
	router.HandleFunc("/{id:[0-9a-zA-Z]+}", show).Methods("GET")
	router.HandleFunc("/pickle", create).Methods("POST")
	http.Handle("/", router)
}

func show(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	id := params["id"]
	if id != "" {
		ctx := appengine.NewContext(r)
		k := datastore.NewKey(ctx, "Pickle", id, 0, nil)
		p := new(Pickle)
		if err := datastore.Get(ctx, k, p); err != nil {
			http.Error(w, "Not found", 404)
			return
		}

		t, err := template.ParseFiles("static/index.html")
		if err != nil {
			log.Println(err)
			http.Error(w, "Couldn't read index.html", 500)
			return
		}
		t.Execute(w, p)
	}
}

func create(w http.ResponseWriter, r *http.Request) {
	defer r.Body.Close()

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Internal error reading request", 500)
		return
	}

	if !isJSON(string(body)) {
		http.Error(w, "Invalid request", 400)
		return
	}

	ctx := appengine.NewContext(r)
	length := 5
	lengthTries := 0

	for length < 10 {
		id := RandString(length)

		if id == "pickle" {
			continue
		}

		p := new(Pickle)
		k := datastore.NewKey(ctx, "Pickle", id, 0, nil)
		err := datastore.Get(ctx, k, p)

		if err == datastore.ErrNoSuchEntity {
			// Free, store here
			p.Data = string(body)
			_, err := datastore.Put(ctx, k, p)
			if err != nil {
				http.Error(w, "Internal error while saving", 500)
				return
			} else {
				// url := fmt.Sprintf("/%s", id)
				// http.Redirect(w, r, url, 301)
				// XmlHttpRequest is bad dealing whilth redirects, so output an {"ok":id} here instead
				w.Header().Set("Content-Type", "application/json")
				fmt.Fprintf(w,"{\"ok\":\"%s\"}", id)
				return
			}
		}

		lengthTries++
		if lengthTries > 10 {
			length++
		}
	}

	http.Error(w, "Out of free slots", 500)
}
