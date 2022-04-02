import * as fs from 'fs';
import * as path from 'path';
import { coolConfig } from '../config/coolConfig';

const { getConfiguredLogChannel, getConfiguredLogFilename } = coolConfig();

export enum LogLevels {
  ERROR = 0,
  WARN = 1,
  INFO = 2,
  DEBUG = 3
}

export type CoolLogChannelName = 'console' | 'file' | 'both';
export type CoolLogChannel = {
  type: CoolLogChannelName;
  level: LogLevels;
  prefixes?: string[];
  fileName?: string;
  timeLabel?: string;
};

const coolLog = (channel: CoolLogChannel) => {
  if (!channel.prefixes) {
    channel.prefixes = ['SERVER'];
  }
  if (!channel.timeLabel) {
    channel.timeLabel = 'AAT';
  }
  const getPrefixes = () => {
    let prefixString = '';
    for (const prefix of channel.prefixes!) {
      prefixString = prefixString + `[${prefix}] `;
    }
    return prefixString;
  };

  const checkLogLevel = (level: LogLevels) => level <= channel.level;

  const levelNames = (level: LogLevels) => {
    switch (level) {
      case LogLevels.ERROR:
        return 'ERROR';
      case LogLevels.WARN:
        return 'WARNING';
      case LogLevels.INFO:
        return 'INFO';
      case LogLevels.DEBUG:
        return 'DEBUG';
    }
  };

  const logsPath = path.join('.', 'logs');

  function buildDateString() {
    const d = new Date();
    return (
      d.getFullYear().toString().padStart(2, '0') +
      '-' +
      (d.getMonth() + 1).toString().padStart(2, '0') +
      '-' +
      d.getDate().toString().padStart(2, '0') +
      ' ' +
      d.getHours().toString().padStart(2, '0') +
      ':' +
      d.getMinutes().toString().padStart(2, '0') +
      ':' +
      d.getSeconds().toString().padStart(2, '0') +
      '.' +
      d.getMilliseconds().toString().padStart(3, '0')
    );
  }

  function buildMessage(data: any[]) {
    const dateString = buildDateString();
    let message = '[' + dateString + '] ';
    for (const d of data) {
      if (typeof d === 'string') {
        message = message + d;
      } else if (typeof d === 'number') {
        message = message + d.toString();
      } else {
        message = message + JSON.stringify(d, null, 4);
      }
      message += ' ';
    }
    message += '\n';
    return message;
  }

  const logChannelFuncMap = {
    console: {
      start: () => console.time(channel.timeLabel),
      end: () => console.timeEnd(channel.timeLabel),
      log: (...data: any[]) => console.timeLog(channel.timeLabel, ...data)
    },
    file: {
      start: () => {
        try {
          fs.unlinkSync(path.join(logsPath, channel.fileName!));
        } catch (e) {
          // do nothing
        }
      },
      end: () => {},
      log: (...data: any[]) => {
        fs.appendFileSync(path.join(logsPath, channel.fileName!), buildMessage(data));
      }
    },
    both: {
      start: () => {
        logChannelFuncMap.console.start();
        logChannelFuncMap.file.start();
      },
      end: () => {
        logChannelFuncMap.file.end();
        logChannelFuncMap.console.end();
      },
      log: (...data: any[]) => {
        logChannelFuncMap.console.log(...data);
        logChannelFuncMap.file.log(...data);
      }
    }
  };

  const start = () => {
    logChannelFuncMap[channel.type].start();
  };

  const log = (level: LogLevels, ...data: any[]) => {
    if (!checkLogLevel(level)) {
      return;
    }
    logChannelFuncMap[channel.type].log(`${getPrefixes()}[${levelNames(level)}]`, ...data);
  };

  const error = (...data: any[]) => {
    log(LogLevels.ERROR, ...data);
  };

  const warn = (...data: any[]) => {
    log(LogLevels.WARN, ...data);
  };

  const info = (...data: any[]) => {
    log(LogLevels.INFO, ...data);
  };

  const debug = (...data: any[]) => {
    log(LogLevels.DEBUG, ...data);
  };

  return { start, error, warn, info, debug };
};

export const aatServiceLog = (name: string) => {
  const channel: CoolLogChannel = {
    type: getConfiguredLogChannel() as CoolLogChannelName,
    level: LogLevels.DEBUG,
    prefixes: [name]
  };
  if (channel.type == 'file') {
    channel.fileName = getConfiguredLogFilename();
  }
  return coolLog(channel);
};

export const aatDefaultLog = () => {
  const channel: CoolLogChannel = {
    type: getConfiguredLogChannel() as CoolLogChannelName,
    level: LogLevels.DEBUG,
    prefixes: ['TEST']
  };
  if (channel.type == 'file') {
    channel.fileName = getConfiguredLogFilename();
  } else {
    channel.level = LogLevels.INFO;
  }
  return coolLog(channel);
};
