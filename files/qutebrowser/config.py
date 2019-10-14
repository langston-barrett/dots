import yaml

with (config.configdir / 'config.yaml').open() as f:
    yaml_data = yaml.safe_load(f)

for k, v in yaml_data.items():
    config.set(k, v)
