let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "921fea4de3f9393cb50e31ec5e6c6b13427bf115";
      sha256 = "0pwp3xyd73lpm6w5xfnwl90h03ybzjshzshxrc26wc6l25afd08p";
    };
  };
in
  sources.reflex-basic-host

