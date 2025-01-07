import path from "path";
import { defineConfig } from 'vite';
import serveStatic from "vite-plugin-serve-static";

export default defineConfig(({ mode }) => {
    const isDev = mode === 'development';

    // When running in dev mode, we want to serve the source files from the local file system.
    // When deployed, the sources files will be served from the `sources` folder in the deployed app.
    const serveStaticPlugin = serveStatic([
        {
            pattern: /^\/sources\/(.*)/,
            resolve: (groups) => path.join("src", groups[1]),
        }
    ]);

    return {
        server: {
            watch: {
                ignored: [
                    "**/*.fs"
                ]
            }
        },
        clearScreen: false,
        plugins: [
            isDev && serveStaticPlugin
        ]
    }
})