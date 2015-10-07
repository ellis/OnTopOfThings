import {fromJS} from 'immutable';
import {patch} from 'immutablepatch';

export default function reducer(state, action) {
	switch (action.type) {
		case 'snapshot':
			for (const item of action.items) {
				state = state.setIn(['items', item.id], fromJS(item));
			}
			break;
		case 'patch1': {
			const id = action.id;
			for (const diff of action.diffs) {
				if (diff[0] === "=") {
					const name = diff[1];
					state = state.setIn(['items', id, name], diff[2]);
				}
			}
			break;
		}
		case 'patchN': {
			for (const hunk of action.hunks) {
				for (const id of hunk.ids) {
					for (const diff of hunk.diffs) {
						if (diff[0] === "=") {
							var name = diff[1];
							state = state.setIn(['items', id, name], diff[2]);
						}
					}
				}
			}
			break;
		}
	}
	return state;
}
