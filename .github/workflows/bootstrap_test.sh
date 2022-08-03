#!/usr/bin/env bash

set -xeu

export LC_ALL=C.UTF-8
export LANG=C.UTF-8

wget -L https://github.com/okteto/okteto/releases/download/2.5.2/okteto-Linux-x86_64 -O okteto
chmod +x ./okteto
./okteto context use --token=${OKTETO_TOKEN} --namespace=${NAMESPACE}
./okteto kubeconfig

kctl () {
  # kubectl --kubeconfig "$KUBECONFIG" "$@"
  kubectl "$@"
}

projdir=$(pwd)
tmpdir=$(mktemp -d /tmp/turma.XXXXXXXXXX)

pushd $tmpdir

mkdir -p manifests

time dhall-to-yaml \
  --omit-empty \
  --documents \
  --file $projdir/.github/workflows/turma-bootstrap-test.dhall \
  > ./manifests/turma-bootstrap-test.yaml

kctl apply -f ./manifests/turma-bootstrap-test.yaml

ssh-keygen -t ed25519 -f keytest -N ""

for pod in node{0..1}; do
  kctl wait --for=condition=Ready --timeout=300s pod/$pod
  kctl cp ./keytest $pod:/root/.ssh/id_ed25519
  kctl cp ./keytest.pub $pod:/root/.ssh/authorized_keys
  kctl exec $pod -- chown root:root /root/.ssh/authorized_keys
done

kctl cp $projdir/_build/prod/decurio*.tar.gz node0:/tmp/
# tar --overwrite may file if the server is running
kctl exec pod/node0 -- bash -c "mkdir -p /decurio && tar -C /decurio -xf /tmp/decurio*.tar.gz && echo 'config Turma.Legionarius, id: \"node0.${NAMESPACE}.svc.cluster.local:19876\"' >> /decurio/releases/*/runtime.exs && /decurio/bin/decurio daemon_iex"
kctl cp $projdir/_build/prod/legionarius*.tar.gz node1:/tmp/

popd
