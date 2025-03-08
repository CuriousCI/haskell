ml-build sources.cm Main.main hs; 
echo "sml @SMLload=${pwd}hs.amd64-linux \$1" > hsml_ls
chmod +x hsml_ls
