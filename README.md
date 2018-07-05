# Distributed collaborative text editing

This is a prototype of a simple distributed collaborative text editor using [brightDB](https://github.com/brightdb/brightdb) and a CRDT based on [ElmSEQ](https://github.com/brightdb/sequence).

## Installation

First download brightDB. It's not in npm yet. So you have to clone it:

    git clone https://github.com/brightdb/brightdb
    cd brightdb
    npm install 
    npm run build

Then `cd` out again and clone this project. Since `brightdb-text` has a relative file reference to `brightdb` in its dependencies, you must check it out in the parent directory of `brightdb`.

    git clone https://github.com/brightdb/brightdb-text
    cd brightdb-text
    npm install
    npm run build

If you have `docker` and `docker-compose` installed, you can run:

    docker-compose up -d

Then point your browser to `localhost:82`.

Alternatively, point it to the `dist` directory of this project.
