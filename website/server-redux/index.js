require('babel/register');

//import makeStore from './src/store.js';
//import startServer from './src/server.js';

//export const store = makeStore();
//startServer(store);

import StateManager from './src/StateManager.js';

const dataDir = "../../../../repo/tasks/data02/";
const state0 = StateManager.loadInitialState(dataDir);
console.log(state0);
