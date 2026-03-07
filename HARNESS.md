## Clone Repository
```bash
git clone https://github.com/pacokwon/petr4 --branch harness
```

## Build Docker Image from `Dockerfile`
```bash
docker build -t petr4-harness -f harness.dockerfile .
```

## Run Container and Open an Interactive Shell
```bash
# Run
docker run -dit --name petr4 petr4-harness

# Open an Interactive Shell
docker exec -it petr4 bash
```

## Typecheck
### Run Positive Tests
```bash
make pos
```

### Run Negative Tests
```bash
make neg
```

## Run V1Model STF Tests
```bash
make sim-v1model
```

## Run eBPF STF Tests
```bash
make sim-ebpf
```
