structure(list(method = "POST", url = "https://api.mistral.ai/v1/chat/completions", 
    status_code = 200L, headers = structure(list(date = "Tue, 15 Oct 2024 22:46:18 GMT", 
        `content-type` = "application/json", `ratelimitbysize-query-cost` = "1027", 
        `x-ratelimitbysize-limit-day` = "150000000", `x-ratelimitbysize-limit-minute` = "500000", 
        `x-ratelimitbysize-remaining-minute` = "498973", `ratelimitbysize-limit` = "500000", 
        `ratelimitbysize-remaining` = "498973", `x-ratelimitbysize-remaining-day` = "149659285", 
        `ratelimitbysize-reset` = "42", `x-envoy-upstream-service-time` = "605", 
        `access-control-allow-origin` = "*", `x-kong-upstream-latency` = "606", 
        `x-kong-proxy-latency` = "2", `x-kong-request-id` = "f447aa1ae71a78273c3fae0a7719122d", 
        `cf-cache-status` = "DYNAMIC", server = "cloudflare", 
        `cf-ray` = "8d33582adda49159-FRA", `content-encoding` = "gzip", 
        `alt-svc` = "h3=\":443\"; ma=86400"), class = "httr2_headers"), 
    body = charToRaw("{\"id\":\"fbe6d2c91760487e9cda2ee15269fcbd\",\"object\":\"chat.completion\",\"created\":1729032378,\"model\":\"mistral-large-latest\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"Hello! How can I assist you today? Let's chat about anything you'd like. 😊\",\"tool_calls\":null},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":6,\"total_tokens\":28,\"completion_tokens\":22}}"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
