# dots

```sh
curl \
  --fail \
  --location \
  --proto '=https' \
  --show-error \
  --silent \
  --tlsv1.2 \
  https://raw.githubusercontent.com/langston-barrett/dots/master/run.sh | \
  bash
```

Pre-commit hook:

```sh
bat <<'EOF' > .git/hooks/pre-commit
#!/usr/bin/env bash
./lint
EOF
chmod +x .git/hooks/pre-commit
```
