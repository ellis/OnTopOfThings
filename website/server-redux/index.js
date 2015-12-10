require('babel/register');

//import makeStore from './src/store.js';
//import startServer from './src/server.js';

//export const store = makeStore();
//startServer(store);

import StateManager from './src/StateManager.js';

const dataDir = "../../../../repo/tasks/data02/";
const state0 = StateManager.loadInitialState(dataDir);
//console.log(JSON.stringify(state0.toJS(), null, '\t'));

import _ from 'lodash';


let order = [];
state0.get('items').forEach((value, key) => {
	//console.log(value.get('created'));
	order.push([value.get('created', '0'), key]);
});
//console.log(order);
order.sort();

function printHistoryStream() {
	for (let pair of order) {
		const id = pair[1];
		const item = state0.getIn(['items', id]).toJS();
		const obj = _.pick(item, _(item).keys().sort().value());
		console.log(JSON.stringify([obj.id, obj.history]));
	}
}

function printDataStream() {
	for (let pair of order) {
		const id = pair[1];
		const item = state0.getIn(['items', id]).toJS();
		//console.log(item);
		const dataKeys = _(item.data).keys().without('history').sort().value();
		const data = _.pick(item.data, dataKeys);
		console.log(JSON.stringify([id, {ver: item.ver, data: data, history: item.history}]));
	}
}

printDataStream();
