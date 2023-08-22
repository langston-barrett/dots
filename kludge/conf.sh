set -ex
rm -f conf.gen.toml
echo "# see conf.sh" > conf.gen.toml
for tool in cargo git; do
  cargo run -q -- zle extract $tool $tool.toml | tee $tool.gen.toml
  cat $tool.gen.toml >> conf.gen.toml
done
sed -i 's/short = "c"/short = "cg"/' conf.gen.toml
cp conf.gen.toml conf.toml
