```
$ ./docker_run.sh ocaml --version
4.05.0
```

```sh
## Build Docker image

docker build \
  --build-arg USER=$USER \
  --build-arg GROUP=$(id -gn) \
  -t vm2gol-v2:ocaml .
```
