let

  region = "ca-central-1";
  accessKeyId = "default"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.micro";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      deployment.ec2.associatePublicIpAddress = true;
      deployment.ec2.ebsInitialRootDiskSize = 8;
    };

in
{ gobble-net = ec2;
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };
}
