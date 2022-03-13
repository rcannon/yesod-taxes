# yesod-taxes
Yesod Web App Federal Income Tax Calculator

## Instructions for Running

1. Clone the repo and move into the created directory:
```
$ git clone https://github.com/rcannon/yesod-taxes.git
$ cd yesod-taxes
```
2. Confirm that Nix Flakes are enabled. 
Build the project:
```
$ nix build
```

3. Activate the Postgres Database:
```
$ ./db_start.sh
```
- Note: to stop the database and remove the stored database files, run `./db_stop.sh`.

4. Run the application:
```
$ nix run
```

5. Open your web browser and navigate to `localhost:3000/`.

6. Follow the instructions on the site!
