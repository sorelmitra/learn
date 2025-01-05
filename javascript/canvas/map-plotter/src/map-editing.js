import { getStore } from './memoize.js';
import { get2dContext } from './canvas-tools.js';

const getMapElementByPosition = (position) => {
  const elements = getStore()['elements'];
  for (let index = 0; index < elements.length; index++) {
    const e = elements[index];
    if (e.isInArea({ elementPosition: e.position, currentPosition: position })) {
      return indexToPointer(index);
    }
  }
  return null;
};

const getMapElementByFields = (fields) => {
  const elements = getStore()['elements'];
  for (let index = 0; index < elements.length; index++) {
    const e = elements[index];
    let found = true;
    for (const field of fields) {
      if (e[field.name] !== field.value) {
        found = false;
      }
    }
    if (found) {
      return indexToPointer(index);
    }
  }
  return null;
};

const getMapElementPointer = ({ where }) => {
  const { position, fields, pointer } = where;
  if (position) return getMapElementByPosition(position);
  if (fields) return getMapElementByFields(fields);
  return pointer;
};

const getMapElement = (options) => {
  const pointer = getMapElementPointer(options);
  const index = pointerToIndex(pointer);
  return getStore()['elements'][index];
};

const drawOnCursor = (drawFunc) => {
  const c = get2dContext();
  c.globalAlpha = 0.5;
  drawFunc({ position: getStore()['cursor'] });
};

export const showCursorOverlay = () => {
  const onCursor = getStore()['onCursor'];
  if (onCursor?.drawFunc && getStore()['isCursorInCanvas'] && !getStore()['forbidPlacement']) {
    drawOnCursor(onCursor.drawFunc);
  }
};

export const plot = () => {
  const c = get2dContext();
  c.globalCompositeOperation = 'source-over';
  c.globalAlpha = 1.0;

  const elements = getStore()['elements'];
  elements.map(e => e.draw(e));
};

export const getElementAdder = ({ draw, isInArea, isMultiPath, anchorToExistingElement }) => () => {
  const element = getMapElement({ where: { pointer: getStore()['mapElementPointer'] } });
  const startPosition = 
    isMultiPath 
    ? anchorToExistingElement
      ? element?.position
      : getStore()['cursor']
    : null;
  let isBeingBuilt = false;
  if (isMultiPath) {
    const existingElement = getMapElement({ where: { fields: [{ name: 'isBeingBuilt', value: true }] } });
    if (existingElement) {
      existingElement.isBeingBuilt = false;
      return;
    }
    isBeingBuilt = true;
  } else {
    isBeingBuilt = false;
  }
  getStore()['elements'].push({
    draw,
    isInArea,
    position: getStore()['cursor'],
    startPosition,
    isBeingBuilt,
  });
};

const indexToPointer = (index) => ++index;
const pointerToIndex = (index) => --index;

export const storeCurrentMapElement = () => {
  const mapElementPointer = getMapElementPointer({ where: { position: getStore()['cursor'] } });
  getStore()['mapElementPointer'] = mapElementPointer;
};

const isOnTarget = (pointer) => (pointer !== null && pointer !== undefined);

const deleteMapElement = (pointer) => {
  if (!isOnTarget(pointer)) return;

  let index = pointerToIndex(pointer);
  getStore()['elements'].splice(index, 1);
};

export const deleteCurrentMapElement = () => {
  const pointer = getStore()['mapElementPointer'];
  deleteMapElement(pointer);
};

export const setForbidPlacement = () => {
  const pointer = getStore()['mapElementPointer'];
  getStore()['forbidPlacement'] = isOnTarget(pointer);
};

