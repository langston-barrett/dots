import dracula.draw
import yaml

config.load_autoconfig(False)

with (config.configdir / 'config.yaml').open() as f:
    yaml_data = yaml.safe_load(f)

for k, v in yaml_data.items():
    config.set(k, v)

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 5,
        'horizontal': 8
    }
})
