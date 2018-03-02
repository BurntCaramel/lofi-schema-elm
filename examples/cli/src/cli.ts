#!/usr/bin/env node
import sade from 'sade'
import * as FS from 'fs'
import { extname } from 'path'
import { promisify } from 'util';
import { startConversion, subscribeToConversion } from './worker'

const readFile = promisify(FS.readFile);

const prog = sade('lofi-schema')

prog
  .version('1.0.0');

prog
  .command('convert <outputFormat>', '', { default: true })
  .describe('Convert from one schema format to another')
  .option('-i, --in', 'The input file to convert')
  .option('-f, --in-format', 'The input format, by default detected from file extension')
  .action(async (outputFormat, opts) => {
    subscribeToConversion((error, result, format) => {
      if (error) {
        process.stderr.write(error.message)
      }
      else {
        process.stdout.write(result || '')
      }
    })

    const inputPath = opts['in']
    const input = (await readFile(inputPath)).toString('utf8');
    const inputFormat = opts['in-format'] || extname(inputPath).slice(1)

    startConversion({
      inputFormat,
      input,
      outputFormat
    });
  });

prog.parse(process.argv);
