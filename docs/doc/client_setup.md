# Client setup

To connect to a Kleio Server a client needs three 
paramenters:

1. the location of the `kleio_home` directory, because
    paths used in API calls must be relative to this directory
2. `kleio_url` the url of the server
3. `kleio_admin_token` an admin token

How to obtain these parameters varies
according to how the server was
started.

## The client started the server

If the client is responsible for starting the kleio
server then these informations should be readily available,
for instance, if the client started the server with:

```console
$ docker run -v /Users/timelink/mhk-home:/kleio-home -e KLEIO_ADMIN_TOKEN=myprivatetoken -p 8088:8088 -d  kleio-server  
```

then `kleio_home` is /Users/timelink/mhk-home,
`kleio_url` is http://localhost:8088 and
`kleio_admin_token` is "myprivatetoken"

The server will generate a file a the root
of `kleio_home` named `.kleio.json` with 
information of the run time environment including
the information necessary for clients to connect:

```json
{
  "keio_version_date":"2023-03-27 04:02:30",
  "kleio_admin_token":"myprivatetoken",
  "kleio_admin_token_path":"/kleio-home/system/conf/kleio/.admin_token",
  "kleio_conf_dir":"/kleio-home/system/conf/kleio",
  "kleio_home":"/kleio-home",
  "kleio_home_local":null,
  "kleio_log":"/kleio-home/system/logs/kleio/kleio_service.log",
  "kleio_setup_date":"2023-03-27T06:40:35+0000",
  "kleio_token_db_status":"Tokens exist",
  "kleio_url":"http://localhost:8088",
  "kleio_version":"11.0",
  "kleio_version_build":"528"
}
```

Note that the kleio server running in docker has no access to the path
of the volume mapped to `/kleio-home` and so there is no value
for `kleio_home_local` . 

To make it easier for clients to discover the location of
the `kleio_home` of a running server the server can be
started setting the `kleio_home_local` variable:

```console
$ docker run -v /Users/timelink/mhk-home:/kleio-home -e KLEIO_HOME_LOCAL=/Users/timelink/mhk-home -e KLEIO_ADMIN_TOKEN=myprivatetoken -p 8088:8088 -d  kleio-server  
```

The server copies the value to the `.kleio.json` file where clients can access it:

```json
{
  "keio_version_date":"2023-03-27 04:02:30",
  "kleio_admin_token":"myprivatetoken",
  "kleio_admin_token_path":"/kleio-home/system/conf/kleio/.admin_token",
  "kleio_conf_dir":"/kleio-home/system/conf/kleio",
  "kleio_home":"/kleio-home",
  "kleio_home_local":"/Users/timelink/mhk-home",
  "kleio_log":"/kleio-home/system/logs/kleio/kleio_service.log",
  "kleio_setup_date":"2023-03-27T06:40:35+0000",
  "kleio_token_db_status":"Tokens exist",
  "kleio_url":"http://localhost:8088",
  "kleio_version":"11.0",
  "kleio_version_build":"528"
}
```
## The client did not start the server

If the client did not start the server, it must locate `kleio-home` directory
and obtain the `kleio_url` and
`kleio_admin_token` from the `.kleio.json` file. 

To do this it can use a file search to locate
the file or use docker commands to
obtain the  `kleio-home`.

## Locating the source of kleio-home with docker tools.

```console
$ # locate the container with volume /kleio-home
$ cid=`docker ps --filter "volume=/kleio-home" --format "{{.ID}}"`
$ echo $cid     

b0d96acf09be

$ # inspect the container for volumes and check the Source parameter

$ docker inspect --format='{{json .Mounts}}' $cid 
```
```json
[{"Type":"bind","Source":"/Users/jrc/mhk-home","Destination":"/kleio-home","Mode":"","RW":true,"Propagation":"rprivate"}]
```

with `jq` installed, in a single line

```console
$ cid=`docker ps --filter "volume=/kleio-home" --format "{{.ID}}"`; docker inspect --format="{{json .Mounts}}" $cid | jq ".[0].Source"

"/Users/jrc/mhk-home"
```

A JavaScript client can use the 
the `dockerode` library to find
the same information.

```JavaScript
const Docker = require('dockerode');
const docker = new Docker();

docker.listContainers({ all: true }, function (err, containers) {
  containers.forEach(function (containerInfo) {
    const container = docker.getContainer(containerInfo.Id);
    container.inspect(function (err, data) {
      if (data.Mounts.some(mount => mount.Name === '/kleio-home')) {
        console.log(`Found container with volume "/kleio-home": ${data.Name}`);
        // get the source of the volume (TBD)
      }
    });
  });
});
```

## Special case of a `mhk` instalation

All the techniques above work in the case
of the `kleio-server` being part of
a `mhk` instalation, but there is a 
shortcut due to the fact that `mhk` creates
a `.mhk` file in the home directory of the
current user. This file contains the 
location of `mhk-home` which is also
`kleio-home`. 

```console
$ cat ~/.mhk

HOST_MHK_USER_HOME="/Users/jrc"
HOST_MHK_HOME="/Users/jrc/mhk-home"
mhk_home_dir="/Users/jrc/mhk-home"
MYSQL_OPTS=" "
KLEIO_SERVER_WORKERS=6
kleio_url=http://127.0.0.1:8088
mhk_url=http://127.0.0.1:8080
portainer_url=http://127.0.0.1:9000
```
Obtaining the server parameters in
a `mhk` instalations in a single line:

```console
$ .  ~/.mhk ; cat $mhk_home_dir/.kleio.json
```
```json

  "keio_version_date":"2023-03-27 14:14:16",
  "kleio_admin_token":"myprivatetoken",
  "kleio_admin_token_path":"/kleio-home/system/conf/kleio/.admin_token",
  "kleio_conf_dir":"/kleio-home/system/conf/kleio",
  "kleio_home":"/kleio-home",
  "kleio_home_local":null,
  "kleio_log":"/kleio-home/system/logs/kleio/kleio_service.log",
  "kleio_setup_date":"2023-03-27T08:46:28+0000",
  "kleio_token_db_status":"Tokens exist",
  "kleio_url":"http://localhost:8088",
  "kleio_version":"11.0",
  "kleio_version_build":"529"
}
```

The `kleio_token` in `mhk` instalations
is set in the file
`mhk-home/system/conf/mhk_system.properties`
in the property `mhk.kleio.service.token.admin`.

If the property does not exist then during
startup a new token is generated and appended
to `mhk-home/system/conf/mhk_system.properties`.

The `kleio-server` will be started with that
same token.

### Dealing with legacy `mhk` instalations.

Instalations before March 2023 do not include
the generation of the `.kleio.json` file.

In this case it is safe to assume that
the property `mhk.kleio.service.token.admin`
in the file `mhk-home/system/conf/mhk_system.properties` contains the `kleio-server` token.






## Special case of a VSCode extension in a `MHK` instalation

The `timelink`  VSCode extension is used
to assist in creating `kleio` source
files that are part of a `mhk` instalation.

In that scenario the VSCode workspace
where the extension will run can vary
according to the specific type of
directory the user is working on.

Note that the techniques above will
also work for the `timelink` extension,
and using the docker interface is the
preferred method. 

The extension can alternatively try
to find `mhk-home`/`kleio-home` by
examining the file system. 

There are several scenarios depending
on the workspace directory location relative
to `mhk-home`.

1. the workspace was opened at the 
   `mhk-home` level.
   * The workspace 
   directory is the `kleio_home` and the `kleio_url` and `kleio_admin_token` can be
   obtained from `.kleio-json`.
2. the workspace was opened in a directory that
  includes `mhk-home` as subdirectory. 
  This can be detected by searching for the
  file `.mhk-home` in the workspace and its
  subdirectories. 
     * If the file is found
  then the directory of the file in `kleio-home`
  and `.kleio-json` provides url and token.
3. the workspace is a subdirectory of a `mhk-home`
  directory (normally 
  `mhk-home/sources` or one of its 
  subdirectorties).
     * this is detected by searching for `.   
        mhk-home` file in the parents of
        the workspace directory.
     * if found hen the directory of the file is
        `kleio-home` and `.kleio-json` provides url and token.
4. none of the above: then the ony way is to
   inquire the docker information as explained
   above.






