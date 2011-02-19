git clone git://github.com/TonyGen/bson-erlang.git bson
cd bson
erlc -o ebin -I include src/*.erl
cd ..

git clone git://github.com/TonyGen/mongodb-erlang.git mongodb
cd mongodb
erlc -o ebin -I include -I .. src/*.erl
cd ..

git clone git://github.com/yrashk/ezmq.git
cd ezmq
make
cd ..

echo "Done"