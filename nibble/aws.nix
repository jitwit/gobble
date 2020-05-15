let region = "ca-central-1";
    accessKeyId = "default"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name
    ec2 = { resources, ... }:
      { deployment = {
          targetEnv = "ec2";
          ec2.accessKeyId = accessKeyId;
          ec2.region = region;
          ec2.instanceType = "t3.nano";
          ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
          ec2.associatePublicIpAddress = true;
          ec2.ebsInitialRootDiskSize = 5;
#          route53.hostName = "boggle-bitch.net";
#          route53.accessKeyId = accessKeyId;
        }; };
in { gobble-net = ec2; resources.ec2KeyPairs.my-key-pair = { inherit region accessKeyId; }; }
