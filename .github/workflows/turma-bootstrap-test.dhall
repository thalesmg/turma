let k8s =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/8c24915f939796277171cd4f4716852de3cc416c/package.dhall
        sha256:532e110f424ea8a9f960a13b2ca54779ddcac5d5aa531f86d82f41f8f18d7ef1

let mkPod
    : Text -> k8s.Pod.Type
    = \(name : Text) ->
        k8s.Pod::{
        , metadata = k8s.ObjectMeta::{
          , name = Some name
          , labels = Some [ { mapKey = "turma/node", mapValue = name } ]
          }
        , spec = Some k8s.PodSpec::{
          , containers =
            [ k8s.Container::{
              , name = "node"
              , image = Some "ubuntu:20.04"
              , command = Some
                [ "bash"
                , "-c"
                , ''
                  apt update
                  DEBIAN_FRONTEND=noninteractive apt install -y ssh openssh-server netcat-openbsd dnsutils
                  echo "AuthorizedKeysFile .ssh/authorized_keys" >> /etc/ssh/sshd_config
                  mkdir -p /root/.ssh/
                  service ssh start
                  tail -f /dev/null
                  ''
                ]
              , ports = Some
                [ k8s.ContainerPort::{ containerPort = +19876 }
                , k8s.ContainerPort::{ containerPort = +19877 }
                , k8s.ContainerPort::{ containerPort = +22 }
                ]
              , readinessProbe = Some k8s.Probe::{
                , exec = Some k8s.ExecAction::{
                  , command = Some [ "ls", "/root/.ssh" ]
                  }
                , initialDelaySeconds = Some +30
                , periodSeconds = Some +10
                }
              }
            ]
          }
        }

let mkService
    : Text -> k8s.Service.Type
    = \(name : Text) ->
        k8s.Service::{
        , metadata = k8s.ObjectMeta::{ name = Some name }
        , spec = Some k8s.ServiceSpec::{
          , ports = Some
            [ k8s.ServicePort::{
              , targetPort = Some (k8s.IntOrString.Int +19876)
              , port = +19876
              , name = Some "router"
              }
            , k8s.ServicePort::{
              , targetPort = Some (k8s.IntOrString.Int +19877)
              , port = +19877
              , name = Some "sub"
              }
            , k8s.ServicePort::{
              , targetPort = Some (k8s.IntOrString.Int +22)
              , port = +22
              , name = Some "ssh"
              }
            ]
          , selector = Some [ { mapKey = "turma/node", mapValue = name } ]
          }
        }

in  [ k8s.Resource.Pod (mkPod "node0")
    , k8s.Resource.Pod (mkPod "node1")
    , k8s.Resource.Service (mkService "node0")
    , k8s.Resource.Service (mkService "node1")
    ]
