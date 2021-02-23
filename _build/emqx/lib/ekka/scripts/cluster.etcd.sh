
erl -pa ebin/ -pa deps/*/ebin -name ekka1@127.0.0.1 -setcookie cookie -config data/app.etcd -s ekka -s ekka autocluster

erl -pa ebin/ -pa deps/*/ebin -name ekka2@127.0.0.1 -setcookie cookie -config data/app.etcd -s ekka -s ekka autocluster

