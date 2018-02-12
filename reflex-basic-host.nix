let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "3ba892320a3fb917aa8772945fab4099b00a8fa4";
      sha256 = "1lhygzw64gmpyqzy7vy4pfpjhcg2jdb6g04mk1c6mcbhcc21na3x";
    };
  };
in
  sources.reflex-basic-host

