# Calls logistics service directly
curl --max-redir 0 -D - -o - --data "$request" --header "Content-Type: application/json" \
      --header "user: $sid" http://$machine:8080/logistics/RPC/EdmInventoryService/GetInventoryByGroupCompanyId


