import { defineConfig } from 'vite';

export default defineConfig({
    //"root": "./public",
    "server": {
        "port": "5173",
        "watch": {
            "ignored": (p => { return p.includes("ace-builds") || p.endsWith(".fs"); }),
            usePolling: true
        }
    },
}) 