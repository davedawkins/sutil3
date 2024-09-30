import { defineConfig } from 'vite';

export default defineConfig({
    "server": {
        "port": "5174",
        "watch": {
            "ignored": (p => { return p.includes("ace-builds") || p.endsWith(".fs"); }),
            usePolling: true
        }
    },
}) 
